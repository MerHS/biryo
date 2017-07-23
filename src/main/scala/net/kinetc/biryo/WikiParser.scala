package net.kinetc.biryo

import org.parboiled2._

import scala.annotation.switch


class WikiParser(val input: ParserInput) extends Parser with StringBuilding {
  type SB = StringBuilder
  type NM = NamuAST.NamuMark
  type PB = NamuAST.ParagraphBuilder
  val NA = NamuAST

  def NamuMarkRule: Rule1[NM] = rule {
    NamuMark ~ EOI
  }

  // Rule 0. Main Rule

  // 강제개행은 제거된다
  def NamuMark: Rule1[NM] = rule {
    push(new PB(Vector[NM](), new SB)) ~ (FetchObject.? ~ findEOIOnce).* ~> ((pb: PB) => NA.pbResolver(pb))
  }

  var foundEOI = false
  private def findEOIOnce = rule {
    NewLine | (&(ch(EOI)) ~ test(!foundEOI) ~ run { foundEOI = true })
  }

  def NamuMarkEndWith(s: String): Rule1[NM] = rule {
    push(new PB(Vector[NM](), new SB)) ~ ((!s ~ FetchObject).? ~ findEOIOnce).* ~> ((pb: PB) => NA.pbResolver(pb))
  }

  def FetchObject = rule {
    (
      LineStartObject ~ run { foundEOI = false } ~> ((pb: PB, lineObj: NM) => NA.pbMerger(pb, lineObj))
    ) |
    (!NewLine ~ FetchChar ~> ((pb: PB, c: Char) => { pb.sb.append(c); pb }))
  }

  /**
    * 한 라인의 시작부분에 있을 때만 의미가 있는 문법 체크
    * @return Headings / Table / Listing / Comment / HR / BlockQuote / Indent
    */
  def LineStartObject: Rule1[NM] = rule {
    run {
      (cursorChar: @switch) match {
        case '\n' | '\r' | '\uFFFF' => MISMATCH // \uFFFF <- Literal EOI
        case ' ' => LineTerm // Indent / List Multiline
        case '=' => Headings | LineTerm
        case '|' => LineTerm // Table Multiline
        case '#' => Redirect | Comment | LineTerm
        case '-' => HR | LineTerm
        case '>' => LineTerm // BlockQuote Multiline
        case _ => LineTerm
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
    (!NewLine ~ FetchChar ~> ((pb: PB, c: Char) => { pb.sb.append(c); pb }))
  }

  def LineObject: Rule1[NM] = rule {
    run {
      (cursorChar: @switch) match {
        case '\n' | '\r' | '\uFFFF' => MISMATCH
        case '{' => HTMLBlock | SpanBlock | RawBlock | StringBox
        case '_' => Underline
        case '-' => StrikeMinus
        case '~' => StrikeTilde
        case '^' => Sup
        case ',' => Sub
        case '_' => Underline
        case '\'' => Bold | Italic
        case '[' => Link // | Macro
        case _ => MISMATCH
      }
    }
  }
  // Rule 8. Indent & Lists (Multi-Liner)

  // Rule 7. Table (Multi-Liner)

  // Rule 6. BlockQuote (Multi-Liner)

  // Rule 5. Macros (Single Bracket)

  // Rule 4. Links & Anchors (Double Brackets)

  def Link = rule { FileLink | DocLink }

  // TODO: [[:파일:example.png]] / [[ 파일:example.png]] 처리 (RawString Link)
  // TODO: parse option to Map
  def FileLink: Rule1[NM] = rule {
    CommandStr("[[파일:") ~
      (LineStringExceptC('|') ~> ((href: String) => NA.FileLink(href, None))) ~
      (
        '|' ~ LineStringExceptS("]]") ~>
        ((fl: NA.FileLink, option: String) => NA.FileLink(fl.href, Some(option)))
      ).? ~ CommandStr("]]")
  }

  def DocLink: Rule1[NM] = rule {
    CommandStr("[[") ~ (LinkPath ~> (NA.DocLink(_, None))) ~ ('|' ~ LinkAlias).? ~ CommandStr("]]")
  }

  def LinkPath: Rule1[NA.NamuHref] = rule {
    ("#s-" ~ (capture(CharPredicate.Digit) ~> (_.toInt)).+('.') ~> NA.SelfParaHref) | // Paragraph of Current Document
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
      ("#s-" ~ (capture(CharPredicate.Digit) ~> (_.toInt)).+('.') ~> NA.ParaHref) |
      ('#' ~ GetUntilAlias ~> NA.AnchorHref) |
      MISMATCH
    )
  }

  def LinkAlias = rule {
    LineTermEndWith("]]") ~> ((link: NA.DocLink, nm: NM) => NA.DocLink(link.href, Some(nm)))
  }

  // Rule 3. Curly Brace Blocks

  // TODO: THIS??
  def HTMLBlock: Rule1[NM] = rule {
    CommandStr("{{{#!html") ~ StringExceptS("}}}") ~ "}}}" ~> NA.HTMLString
  }

  def SpanBlock = rule { ColorRGBBlock | ColorTextBlock | SizeBlock }

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

  def StringBox = rule { MatchBlock("{{|", "|}}") ~> NA.StringBox }

  def RawBlock: Rule1[NM] = rule {
      CommandStr("{{{") ~ push(new SB) ~ RBResolver.* ~ "}}}" ~>
        ((tsb: SB) => NA.InlineString(tsb.toString))
  }

  /// 단일 역슬래시도 그대로 출력해야함
  private def RBResolver = rule {
    (CurlyBraceBlock | ( !"}}}" ~ capture(ANY))) ~>
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

  def Redirect = rule { ("#redirect" | "#넘겨주기") ~ WL ~ LinkPath ~> NA.Redirect }
  def Comment = rule { "##" ~ LineString ~> NA.Comment }
  def HR = rule { (4 to 10).times(ch('-')) ~ &(NewLine | EOI) ~ push(NA.HR) }

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

  def CommandStr(s: String) = rule { !('\\' ~ s) ~ atomic(s) }
  def FetchChar: Rule1[Char] = rule {
    ('\\' ~ ANY ~ push(lastChar)) | (ANY ~ push(lastChar))
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

  def LineString = StringExceptSPred("\n\r")

  def WL = rule { anyOf(" \t").* }
  def NewLine = rule { '\r'.? ~ '\n' }
}
