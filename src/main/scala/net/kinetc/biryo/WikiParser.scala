package net.kinetc.biryo

import org.parboiled2._
import shapeless.HNil

import scala.annotation.switch
import scala.util.{Failure, Success}


class WikiParser(val input: ParserInput) extends Parser with StringBuilding {
  type SB = StringBuilder
  type NM = NamuAST.NamuMark
  type PB = NamuAST.ParagraphBuilder
  val NA = NamuAST

  def NamuMarkRule: Rule1[NM] = rule {
    NamuMark ~ EOI
  }

  var parserSuccess = false

  def SubParser = rule {
    (MATCH ~> ((s: String) =>
      new WikiParser(s).NamuMarkRule.run() match {
        case Success(result) => parserSuccess = true; result
        case Failure(e) => parserSuccess = false; NA.BR
      })) ~ test(parserSuccess)
  }

  // Rule 0. Main Rule

  // 강제개행은 제거된다
  def NamuMark: Rule1[NM] = rule {
    push(new PB(Vector[NM](), new SB)) ~ (FetchObject.* ~ findEOIOnce).* ~> ((pb: PB) => NA.pbResolver(pb))
  }

  var foundEOI = false
  var findEnd = false
  private def findEOIOnce = rule {
    (NewLine ~> ((pb: PB) => NA.pbMerger(pb, NA.BR))) |
      (&(ch(EOI)) ~ test(!foundEOI) ~ run { foundEOI = true } ~> ((pb: PB) => pb))
  }
  private def findEndWithOnce(s: String) = rule {
    findEOIOnce |
      (&(s) ~ test(!findEnd) ~ run { findEnd = true } ~> ((pb: PB) => pb))
  }

  def NamuMarkEndWith(s: String): Rule1[NM] = rule {
    push(new PB(Vector[NM](), new SB)) ~
      ((!s ~ FetchObjectEW(s)).* ~ findEndWithOnce(s)).* ~>
      ((pb: PB) => NA.pbResolver(pb))
  }

  def FetchObject = rule {
    (
      LineStartObject ~ run { findEnd = false; foundEOI = false } ~>
        ((pb: PB, lineObj: NM) => NA.pbMerger(pb, lineObj))
    ) |
    (!CheckLineEnd ~ FetchChar ~> ((pb: PB, c: Char) => { pb.sb.append(c); pb }))
  }

  def FetchObjectEW(s: String) = rule {
    (
      LineStartObjectEW(s) ~ run { findEnd = false; foundEOI = false } ~>
        ((pb: PB, lineObj: NM) => NA.pbMerger(pb, lineObj))
    ) |
    (!CheckLineEnd ~ FetchChar ~> ((pb: PB, c: Char) => { pb.sb.append(c); pb }))
  }

  /**
    * 한 라인의 시작부분에 있을 때만 의미가 있는 문법 체크
    * @return Headings / Table / Listing / Comment / HR / BlockQuote / Indent
    */
  def LineStartObject: Rule1[NM] = rule {
    run {
      (cursorChar: @switch) match {
        case '\n' | '\r' | '\uFFFF' => MISMATCH // \uFFFF <- Literal EOI
        case ' ' => Indent | LineTerm // Indent / List Multiline
        case '=' => Headings | LineTerm
        case '|' => Table | LineTerm // Table Multiline
        case '#' => Redirect | Comment | LineTerm
        case '-' => HR | LineTerm
        case '>' => BlockQuote | LineTerm
        case '[' => LineStartMacro | LineTerm
        case _ => LineTerm
      }
    }
  }
  def LineStartObjectEW(s: String): Rule1[NM] = rule {
    run {
      (cursorChar: @switch) match {
        case '\n' | '\r' | '\uFFFF' => MISMATCH
        case ' ' => IndentEW(s) | LineTermEndWith(s)
        case '=' => Headings | LineTermEndWith(s)
        case '|' => Table | LineTermEndWith(s)
        case '#' => Redirect | Comment | LineTermEndWith(s)
        case '-' => HR | LineTermEndWith(s)
        case '>' => BlockQuoteEW(s) | LineTermEndWith(s)
        case '[' => LineStartMacro | LineTermEndWith(s)
        case _ => LineTermEndWith(s)
      }
    }
  }

  // RuleX. Single Line Parser

  /**
    * 한 라인 안에만 있을 수 있는 문법 체크
    * @return Bold / Italic / etc...
    */
  def LineTerm: Rule1[NM] = rule {
    push(new PB(Vector[NM](), new SB)) ~ FetchLineObject.* ~> ((pb: PB) => NA.pbResolver(pb))
  }

  def LineTermEndWith(s: String): Rule1[NM] = rule {
    push(new PB(Vector[NM](), new SB)) ~ (!s ~ FetchLineObject).* ~> ((pb: PB) => NA.pbResolver(pb))
  }

  def FetchLineObject = rule {
    (
      LineObject ~> ((pb: PB, lineObj: NM) => NA.pbMerger(pb, lineObj))
    ) |
    (!CheckLineEnd ~ FetchChar ~> ((pb: PB, c: Char) => { pb.sb.append(c); pb }))
  }

  def LineObject: Rule1[NM] = rule {
    run {
      (cursorChar: @switch) match {
        case '\n' | '\r' | '\uFFFF' => MISMATCH
        case '{' => SpecialBlock | SpanBlock | RawBlock | WordBox
        case '_' => Underline
        case '-' => StrikeMinus
        case '~' => StrikeTilde
        case '^' => Sup
        case ',' => Sub
        case '_' => Underline
        case '\'' => Bold | Italic
        case '[' => OtherMacro | FootNote | LinkOnlyFN | Link | FootNoteList
        case _ => MISMATCH
      }
    }
  }
  // Rule 9. Indent & Lists (Multi-Liner)

  // TODO: Check it Before get indent
  def ListObj: Rule1[NM] = ???

  def IndentEW(s: String): Rule1[NM] = rule {
    CheckIndent ~ test(spaceNo > 0) ~ IndentLineEW(s).+ ~>
      ((size: Int, sl: Seq[String]) => {
        NA.Indent(new WikiParser(sl.mkString).NamuMarkRule.run().get, size)
      })
  }

  private def IndentLineEW(st: String): Rule1[String] = rule {
    spaceNo.times(ch(' ')) ~ LineStringExceptS(st) ~
      ((&(ch(EOI) | st) ~> ((s: String) => s)) |
        (NewLine ~> ((s: String) => s + '\n')))
  }

  def Indent: Rule1[NM] = rule {
    CheckIndent ~ test(spaceNo > 0) ~ IndentLine.+ ~>
      ((size: Int, sl: Seq[String]) => {
        NA.Indent(new WikiParser(sl.mkString).NamuMarkRule.run().get, size)
      })
  }

  // I Hate Type System....
  private def IndentLine: Rule1[String] = rule {
    spaceNo.times(ch(' ')) ~ LineString ~
      ((&(EOI) ~> ((s: String) => s)) |
        (NewLine ~> ((s: String) => s + '\n')))
  }

  var spaceNo: Int = 0

  private def CheckIndent: Rule1[Int] = rule {
    &(capture(ch(' ').+) ~> ((s: String) => {
      spaceNo = s.length
    })) ~
      push(spaceNo)
  }

  // Rule 8. Table (Multi-Liner)

  def Table: Rule1[NM] = rule {
    TableHeader ~
      (
        TR ~> ((tw: NA.TableWrapper, tr: NA.TR) =>
          NA.TableWrapper(NA.Table(tw.value.valueSeq :+ tr, tw.value.styles), tw.caption))
        ).*
  }

  def TableHeader: Rule1[NA.TableWrapper] = rule {
    TRWithCaption ~> ((nm: NM, tr: NA.TR) =>
      if (nm == NA.RawString(""))
        NA.TableWrapper(NA.Table(Vector[NA.TR](tr), List[NA.TableStyle]()), None)
      else
        NA.TableWrapper(NA.Table(Vector[NA.TR](tr), List[NA.TableStyle]()), Some(nm))
      )
  }

  def TRWithCaption: Rule2[NM, NA.TR] = rule {
    (TDWithCaption ~> ((td: NA.TD) => List[NA.TD](td))) ~
      (!CheckTableEnd ~ TD ~> ((tdl: List[NA.TD], td: NA.TD) => td :: tdl)).* ~
      FetchTableEnd ~>
      ((tdl: List[NA.TD]) => NA.TR(tdl.reverse, List[NA.TableStyle]()))
  }

  def TR: Rule1[NA.TR] = rule {
    push(List[NA.TD]()) ~
      (TD ~ !CheckTableEnd ~> ((tdl: List[NA.TD], td: NA.TD) => td :: tdl)).* ~
      TD ~ FetchTableEnd ~>
      ((tdl: List[NA.TD], td: NA.TD) => NA.TR((td :: tdl).reverse, List[NA.TableStyle]()))
  }

  // TODO: "||||||" ~ (역방향으로 Fetch)
  // push(NA.ColSpan(0)) ~ ("||" ~> ((cs: NA.ColSpan) => NA.ColSpan(cs.value + 1)).+
  def TD: Rule1[NA.TD] = rule {
    FetchTDColSpan ~ FetchTableData
  }

  def TDWithCaption: Rule2[NM, NA.TD] = rule {
    FetchTDWithCaption ~ FetchTableData
  }

  def FetchTableData = rule {
    FetchTableCSSList ~ FetchTableString ~ SubParser ~>
      ((tsl: List[NA.TableStyle], nm: NM) => NA.TD(nm, tsl))
  }

  def FetchTableString = rule {
    FetchTableRawString ~> ((tsl: List[NA.TableStyle], s: String) =>
      if (s.length == 0) {
        tsl :: s :: HNil
      } else if (s.length >= 2 && s(0) == ' ' && s(s.length - 1) == ' ') {
        (NA.Align(NA.AlignCenter, forTable = false) :: tsl) :: s.substring(1, s.length - 1) :: HNil
      } else if (s(0) == ' ') {
        (NA.Align(NA.AlignRightBottom, forTable = false) :: tsl) :: s.substring(1) :: HNil
      } else if (s(s.length - 1) == ' ') {
        (NA.Align(NA.AlignLeftTop, forTable = false) :: tsl) :: s.substring(0, s.length - 1) :: HNil
      } else {
        tsl :: s :: HNil
      }
      )
  }

  def FetchTableEnd: Rule0 = rule {
    CommandStr("||") ~ ch('|').* ~ FetchLineEnd
  }

  def CheckTableEnd: Rule0 = rule {
    &(CommandStr("||") ~ ch('|').* ~ CheckWLLineEnd)
  }

  def FetchTableRawString: Rule1[String] = rule {
    capture((!CommandStr("||") ~ ANY).*) ~ !EOI
  }

  def FetchTDColSpan: Rule1[List[NA.TableStyle]] = rule {
    push(List[NA.TableStyle]()) ~ push(NA.ColSpan(0)) ~
      ("||" ~ run((cs: NA.ColSpan) => NA.ColSpan(cs.value + 1))).+ ~>
      ((tsl: List[NA.TableStyle], cs: NA.ColSpan) =>
        if (cs.value <= 1) tsl else cs :: tsl)
  }

  def FetchTDWithCaption: Rule2[NM, List[NA.TableStyle]] = rule {
    ('|' ~ LineTermEndWith("|") ~ '|') ~ push(List[NA.TableStyle]()) ~ push(NA.ColSpan(1)) ~
      ("||" ~ run((cs: NA.ColSpan) => NA.ColSpan(cs.value + 1))).* ~>
      ((tsl: List[NA.TableStyle], cs: NA.ColSpan) =>
        if (cs.value <= 1) tsl else cs :: tsl)
  }

  def FetchTableCSSList = rule {
    (TableCSS ~> ((tsl: List[NA.TableStyle], ts: NA.TableStyle) => ts :: tsl)).*
  }

  def TableCSS: Rule1[NA.TableStyle] = rule {
    !"\\<" ~ '<' ~ (
      (
        // Parsing Table Style
        ignoreCase("table") ~ WL.? ~
          (
            (ignoreCase("bordercolor=") ~ UnquoteStr ~> (v => NA.BorderColor(v, forTable = true))) |
            (ignoreCase("bgcolor=") ~ UnquoteStr ~> (v => NA.BgColor(v, forTable=true))) |
            (ignoreCase("align=") ~ UnquoteStr ~>
              ((v: String) => {
                if (v.equalsIgnoreCase("center"))
                  NA.Align(NA.AlignCenter, forTable=true)
                else if (v.equalsIgnoreCase("right"))
                  NA.Align(NA.AlignRightBottom, forTable=true)
                else
                  NA.Align(NA.AlignLeftTop, forTable=true)
              })) |
            (ignoreCase("width=") ~ UnquoteStr ~> (v => NA.Width(v, forTable=true))) |
            (ignoreCase("height=") ~ UnquoteStr ~> (v => NA.Height(v, forTable=true)))
          )
      ) |
      (
        // Parsing Table Cell Span
        ('-' ~ capture(CharPredicate.Digit.+) ~ '>' ~> (v => NA.ColSpan(v.toInt))) |
        ('|' ~ capture(CharPredicate.Digit.+) ~ '>' ~> (v => NA.RowSpan(v.toInt, NA.AlignCenter))) |
        ("^|" ~ capture(CharPredicate.Digit.+) ~ '>' ~> (v => NA.RowSpan(v.toInt, NA.AlignLeftTop))) |
        ("v|" ~ capture(CharPredicate.Digit.+) ~ '>' ~> (v => NA.RowSpan(v.toInt, NA.AlignRightBottom)))
      ) |
      (
        // Parsing Table Cell Text-Align
        (":>" ~ push(NA.Align(NA.AlignCenter, forTable=false))) |
        (")>" ~ push(NA.Align(NA.AlignRightBottom, forTable=false))) |
        ("(>" ~ push(NA.Align(NA.AlignLeftTop, forTable=false)))
      ) |
      (
        // Parsing Table Cell Style
        (ignoreCase("bordercolor=") ~ UnquoteStr ~> (v => NA.BorderColor(v, forTable = false))) |
        (ignoreCase("bgcolor=") ~ UnquoteStr ~> (v => NA.BgColor(v, forTable=false))) |
        (ignoreCase("width=") ~ UnquoteStr ~> (v => NA.Width(v, forTable=false))) |
        (ignoreCase("height=") ~ UnquoteStr ~> (v => NA.Height(v, forTable=false)))
      ) |
        // MISMATCHED => Fallback to Table Cell Color
      (UnquoteStr ~> (v => NA.BgColor(v, forTable=false)))
    )
  }

  private def UnquoteStr: Rule1[String] = rule {
    ('\"' ~ StringExceptSPred("\">\n\r") ~ "\">") |
    ('\'' ~ StringExceptSPred("\'>\n\r") ~ "\'>") |
    (StringExceptSPred(">\n\r") ~ ">")
  }

  // Rule 7. BlockQuote (Multi-Liner)

  def BlockQuote: Rule1[NM] = rule {
    BlockQuoteLine.+ ~>
      ((sl: Seq[String]) =>
        NA.BlockQuote(new WikiParser(sl.mkString).NamuMarkRule.run().get))
  }

  def BlockQuoteEW(s: String): Rule1[NM] = rule {
    BlockQuoteLineEW(s).+ ~>
      ((sl: Seq[String]) =>
        NA.BlockQuote(new WikiParser(sl.mkString).NamuMarkRule.run().get))
  }

  // I Hate Type System....
  private def BlockQuoteLine: Rule1[String] = rule {
    '>' ~ LineString ~
      ((&(EOI) ~> ((s: String) => s)) |
      (NewLine ~> ((s: String) => s + '\n')))
  }

  private def BlockQuoteLineEW(st: String): Rule1[String] = rule {
    '>' ~ LineStringExceptS(st) ~
      ((&(ch(EOI) | st) ~> ((s: String) => s)) |
        (NewLine ~> ((s: String) => s + '\n')))
  }

  // Rule 6. FootNote (Single Bracket)

  def FootNote: Rule1[NM] = rule {
    CommandStr("[*") ~ LineStringExceptC(' ') ~ ' ' ~
      LineTermEndWith("]") ~ CommandStr("]") ~>
      ((noteStr: String, value: NM) => {
        if (noteStr == "")
          NA.FootNote(value, None)
        else
          NA.FootNote(value, Some(noteStr))
      })
  }

  def LinkOnlyFN: Rule1[NM] = rule {
    CommandStr("[*") ~ LineStringExceptSPred(" ]") ~ ']' ~> NA.LinkOnlyFN
  }

  // Rule 5. Macros (Single Bracket)
  // 매크로는 대소문 구분 X

  def LineStartMacro: Rule1[NM] = rule { FootNoteList | TableOfContents }
  def OtherMacro: Rule1[NM] = rule { BR | Include | DateMacro | Anchor | YoutubeLink }

  def FootNoteList = rule {
    (CommandStr("[각주]") | ICCommandStr("[footnote]")) ~ WL.? ~ push(NA.FootNoteList)
  }
  def TableOfContents = rule {
    (CommandStr("[목차]") | ICCommandStr("[tableofcontents]")) ~ WL.? ~ CheckLineEnd ~ push(NA.TableOfContents)
  }

  def Include = rule {
    ICCommandStr("[include(") ~ LineStringExceptS(")]") ~ CommandStr(")]") ~>
      ((argString: String) => {
        val args = argString.split(',')
        NA.Include(args.head.trim, argParse(args.tail))
      })
  }
  def BR = rule { ICCommandStr("[br]") ~ push(NA.BR) }
  def Age = rule { ICCommandStr("[age(") ~ LineStringExceptC(')') ~ ")]" ~> NA.AgeMacro }
  def DateMacro = rule { (ICCommandStr("[date]") | ICCommandStr("[datetime]")) ~ push(NA.DateMacro) }
  def Anchor = rule { ICCommandStr("[anchor(") ~ LineStringExceptC(')') ~ CommandStr(")]") ~> NA.Anchor }
  def YoutubeLink = rule {
    ICCommandStr("[youtube(") ~ capture(noneOf(",)\n").+).+(',') ~ CommandStr(")]") ~>
      ((args: Seq[String]) => NA.YoutubeLink(args.head.trim, argParse(args.tail)))
  }


  // Rule 4. Links & Anchors (Double Brackets)

  def Link: Rule1[NM] = rule { FileLink | DocType | DocLink }

  def DocType: Rule1[NM] = rule {
    CommandStr("[[분류:") ~ LineStringExceptS("]]") ~ CommandStr("]]") ~> NA.DocType
  }

  def FileLink: Rule1[NM] = rule {
    CommandStr("[[파일:") ~
      (LineStringExceptC('|') ~> ((href: String) => NA.FileLink(href, Map[String, String]()))) ~
      (
        '|' ~ LineStringExceptS("]]") ~>
        ((fl: NA.FileLink, option: String) => NA.FileLink(fl.href, argParse(option)))
      ).? ~ CommandStr("]]")
  }

  def DocLink: Rule1[NM] = rule {
    (CommandStr("[[:") | CommandStr("[[ ") | CommandStr("[[")) ~
      (LinkPath ~> (NA.DocLink(_, None))) ~ ('|' ~ LinkAlias).? ~ CommandStr("]]")
  }

  def LinkPath: Rule1[NA.NamuHref] = rule {
    ("#s-" ~ (capture(CharPredicate.Digit.+) ~> (_.toInt)).+('.') ~> NA.SelfParaHref) | // Paragraph of Current Document
    ('#' ~ GetUntilAlias ~> NA.SelfAnchorHref) | // Anchor of Current Document
    (&("http://" | "https://") ~ GetUntilAlias ~> NA.ExternalHref) |
    ("../" ~ &("]]" | '|') ~ push(NA.SuperDocHref)) |
    ('/' ~ NormalLinkPath ~> NA.ChildDocHref) |
    NormalLinkPath // Normal Link
  }

  // |, ]]
  private def GetUntilAlias: Rule1[String] = rule {
    clearSB() ~ (!(CheckS("]]") | CheckSPred("|\n\r")) ~ Character).+ ~ push(sb.toString)
  }

  def NormalLinkPath: Rule1[NA.NamuHref] = rule {
    (clearSB() ~ (!(CheckS("]]") | CheckSPred("#|\n\r")) ~ Character).+ ~ push(sb.toString)) ~ (
      (&("]]" | '|') ~> NA.NormalHref) |
      ("#s-" ~ (capture(CharPredicate.Digit.+) ~> (_.toInt)).+('.') ~> NA.ParaHref) |
      ('#' ~ GetUntilAlias ~> NA.AnchorHref) |
      MISMATCH
    )
  }

  private def LinkAlias = rule {
    LineTermEndWith("]]") ~> ((link: NA.DocLink, nm: NM) => NA.DocLink(link.href, Some(nm)))
  }

  // Rule 3. Curly Brace Blocks

  def SpecialBlock: Rule1[NM] = rule { SyntaxBlock | WikiBlock | HTMLBlock }

  def SyntaxBlock: Rule1[NM] = rule {
    ICCommandStr("{{{#!syntax") ~ WL.? ~ SingleWord ~ WL.? ~ NewLine.? ~
      capture((!"}}}" ~ ANY).*) ~ "}}}" ~> NA.SyntaxBlock
  }

  // {{{#!wiki style="height=300" [[Markup]]}}} 등
  def WikiBlock: Rule1[NM] = rule {
    ICCommandStr("{{{#!wiki") ~ WL.? ~ "style=\"" ~
      StringExceptC('"') ~ '"' ~ WL.? ~ NewLine.? ~
      NamuMarkEndWith("}}}") ~ CommandStr("}}}") ~>
      NA.WikiBlock
  }

  def HTMLBlock: Rule1[NM] = rule {
    ICCommandStr("{{{#!html") ~ WL.? ~ capture((!"}}}" ~ ANY).*) ~ "}}}" ~> NA.HTMLString
  }

  def SpanBlock: Rule1[NM] = rule {
    ColorRGBBlock | ColorTextBlock | SizeBlock
  }

  def ColorRGBBlock = rule {
    CommandStr("{{{#") ~
      (
        (capture(3.times(CharPredicate.HexDigit)) ~ ' ') |
        (capture(6.times(CharPredicate.HexDigit)) ~ ' ')
      ) ~
      LineTermEndWith("}}}") ~ CommandStr("}}}") ~> ((s: String, nm: NM) => NA.ColorBlock(nm, "#" + s))
  }

  def ColorTextBlock = rule {
    CommandStr("{{{#") ~
      (
        capture(CharPredicate.Alpha.+) ~ ' '
      ) ~
      LineTermEndWith("}}}") ~ CommandStr("}}}") ~> ((s: String, nm: NM) => NA.ColorBlock(nm, s))
  }

  def SizeBlock: Rule1[NM] = rule {
    CommandStr("{{{+") ~ capture(CharPredicate("12345")) ~ ' ' ~
      LineTermEndWith("}}}") ~ CommandStr("}}}") ~>
      ((s: String, nm: NM) => NA.SizeBlock(nm, s.toInt))
  }

  def WordBox = rule { MatchBlock("{{|", "|}}") ~> NA.WordBox }

  var isMultiLine = false
  def RawBlock: Rule1[NM] = rule {
    ((CommandStr("{{{") ~ WL.? ~ NewLine ~ run {
      isMultiLine = true
    }) |
      (CommandStr("{{{") ~ run {
        isMultiLine = false
      })) ~
      push(new SB) ~ RBResolver.* ~
      (("\n}}}" ~ run {
        isMultiLine = true
      }) | "}}}") ~>
      ((tsb: SB) => NA.InlineString(tsb.toString, isMultiLine))
  }

  /// 단일 역슬래시도 그대로 출력해야함
  private def RBResolver = rule {
    (CurlyBraceBlock | ( !("\n}}}" | "}}}") ~ capture(ANY))) ~>
      ((tsb: SB, s: String) => { tsb.append(s); tsb })
  }

  def CurlyBraceBlock: Rule1[String] = rule {
    '{' ~ push(new SB) ~ CBResolver.* ~ '}' ~>
      ((tsb: SB) => { tsb.append('}'); '{' + tsb.toString })
  }

  private def CBResolver = rule {
    (CurlyBraceBlock | (!ch('}') ~ capture(ANY))) ~>
      ((tsb: SB, s: String) => { tsb.append(s); tsb })
  }


  // Rule 2. Basic Blocks / One-liners

  def Redirect = rule { ("#redirect" | "#넘겨주기") ~ WL ~ LineString ~> NA.Redirect }

  def Comment = rule {
    "##" ~ LineString ~ FetchLineEnd ~> NA.Comment
  }

  def HR = rule {
    (4 to 10).times(ch('-')) ~ FetchLineEnd ~ push(NA.HR)
  }

  def Headings = rule { H6 | H5 | H4 | H3 | H2 | H1 }

  def H1 = rule { LineMatchBlock("= ", " =") ~ CheckLineEnd ~> (NA.RawHeadings(_, 1))}
  def H2 = rule { LineMatchBlock("== ", " ==") ~ CheckLineEnd ~> (NA.RawHeadings(_, 2))}
  def H3 = rule { LineMatchBlock("=== ", " ===") ~ CheckLineEnd ~> (NA.RawHeadings(_, 3))}
  def H4 = rule { LineMatchBlock("==== ", " ====") ~ CheckLineEnd ~> (NA.RawHeadings(_, 4))}
  def H5 = rule { LineMatchBlock("===== ", " =====") ~ CheckLineEnd ~> (NA.RawHeadings(_, 5))}
  def H6 = rule { LineMatchBlock("====== ", " ======") ~ CheckLineEnd ~> (NA.RawHeadings(_, 6))}

  def StrikeMinus = rule { LineBlock("--") ~> NA.Strike }
  def StrikeTilde = rule { LineBlock("~~") ~> NA.Strike }
  def Sup = rule { LineBlock("^^") ~> NA.Sup }
  def Sub = rule { LineBlock(",,") ~> NA.Sub }
  def Underline = rule { LineBlock("__") ~> NA.Underline }
  def Bold = rule { LineBlock("'''") ~> NA.Bold }
  def Italic = rule { LineBlock("''") ~> NA.Italic }

  def LineBlock(s: String) = rule { CommandStr(s) ~ LineTermEndWith(s) ~ CommandStr(s)  }
  def LineMatchBlock(head: String, tail: String) =
    rule { CommandStr(head) ~ LineTermEndWith(tail) ~ CommandStr(tail) }

  def Block(s: String) = rule { CommandStr(s) ~ NamuMarkEndWith(s) ~ CommandStr(s)  }
  def MatchBlock(head: String, tail: String) =
    rule { CommandStr(head) ~ NamuMarkEndWith(tail) ~ CommandStr(tail) }

  // Rule 1. Basic Characters

  def FetchLineEnd = rule {
    WL.? ~ (NewLine | &(EOI))
  }

  def ICCommandStr(s: String) = rule { !('\\' ~ ignoreCase(s)) ~ atomic(ignoreCase(s)) }
  def CommandStr(s: String) = rule { !('\\' ~ s) ~ atomic(s) }
  def FetchChar: Rule1[Char] = rule {
    ('\\' ~ ANY ~ push(lastChar)) | (ANY ~ push(lastChar))
  }

  def CheckWLLineEnd = rule {
    &(WL.? ~ (NewLine | EOI))
  }
  def CheckLineEnd = rule { &(NewLine | EOI) }
  def CheckC(c: Char) = rule { &(!('\\' ~ c) ~ c) }
  def CheckS(s: String) = rule { &(!('\\' ~ s) ~ s) }
  def CheckSPred(s: String) = rule { &(!('\\' ~ anyOf(s)) ~ anyOf(s)) }

  def NormalChar = rule { !ch('\\') ~ ANY ~ appendSB() }
  def QuotedChar = rule { ('\\' ~ NormalChar) | ("\\\\" ~ appendSB()) }
  def Character = rule { NormalChar | QuotedChar}
  def NormalString = rule { clearSB() ~ Character.* ~ push(sb.toString) }

  // \ (backslash) 처리한 Char은 읽어들임
  def CharExceptC(c: Char) = rule { !(!('\\' ~ c) ~ c) ~ (NormalChar | QuotedChar) }
  def StringExceptC(c: Char) = rule { clearSB() ~ CharExceptC(c).* ~ push(sb.toString) }
  def LineStringExceptC(c: Char) = rule{ clearSB() ~ (!CheckLineEnd ~ CharExceptC(c)).* ~ push(sb.toString) }
  def CharExceptS(s: String) = rule { !(!('\\' ~ s) ~ s) ~ (NormalChar | QuotedChar) }
  def StringExceptS(s: String) = rule { clearSB() ~ CharExceptS(s).* ~ push(sb.toString) }
  def LineStringExceptS(s: String) = rule{ clearSB() ~ (!CheckLineEnd ~ CharExceptS(s)).* ~ push(sb.toString) }
  def CharExceptSPred(s: String) = rule { !(!('\\' ~ anyOf(s)) ~ anyOf(s)) ~ (NormalChar | QuotedChar) }
  def StringExceptSPred(s: String) = rule { clearSB() ~ CharExceptSPred(s).* ~ push(sb.toString) }
  def LineStringExceptSPred(s: String) = rule{ clearSB() ~ (!CheckLineEnd ~ CharExceptSPred(s)).* ~ push(sb.toString) }

  def SingleWord = StringExceptSPred(" \t\n\r")
  def LineString = StringExceptSPred("\n\r")

  def WS = rule { anyOf(" \t\r\n").* }
  def WL = rule { anyOf(" \t").* }
  def NewLine = rule { '\r'.? ~ '\n' }

  private def argParse(argString: String, argDelim: Char='&', equalSign: Char='='): Map[String, String] = {
    var argMap = Map[String, String]()
    for(arg <- argString.split(argDelim)) {
      val argSplit = arg.split(equalSign)
      argMap += argSplit(0).trim -> (if (argSplit.length >= 2) argSplit(1).trim else "")
    }
    argMap
  }

  // Seq("a=3", "X", "b=5", "c=6") => Map("a" -> "3,X", "b" -> "5", "c" -> 6)
  private def argParse(args: Seq[String]): Map[String, String] = {
    var argMap = Map[String, String]()
    var newArgs = List[String]()

    for (arg <- args) {
      if (arg.contains("=") || newArgs.isEmpty) {
        newArgs = arg :: newArgs
      } else {
        newArgs = (newArgs.head + "," + arg) :: newArgs.tail
      }
    }

    for (arg <- newArgs) {
      val argSplit = arg.split("=", 2)
      argMap += argSplit(0).trim -> (if (argSplit.length >= 2) argSplit(1).trim else "")
    }
    argMap
  }
}
