
import net.kinetc.biryo.{ASTPostProcessor, HTMLRenderer, NamuAST, WikiParser}
import org.parboiled2._
import org.specs2.mutable.Specification

import scala.io.Source
import scala.util.{Failure, Success, Try}

class WikiParserSpec extends Specification {
  type NM = NamuAST.NamuMark
  type PG = NamuAST.Paragraph
  val RS = NamuAST.RawString
  val IS = NamuAST.InlineString
  val NA = NamuAST

  "WikiParser" should {
    "parse Basic Characters" in {
      var parser = new WikiParser("test")
      parse(parser, parser.FetchChar.run()) === 't'

      parser = new WikiParser("\\quoted")
      parse(parser, parser.FetchChar.run()) === 'q'

      parser = new WikiParser("qu\\o\n\\ted")
      parse(parser, parser.NormalString.run()) === "quo\nted"

      parser = new WikiParser("[\\]\\{[{}[]")
      parse(parser, parser.StringExceptC('{').run()) === "[]{["

      parser = new WikiParser("test\\testtest")
      parse(parser, parser.StringExceptS("tt").run()) == "testtes"

      parser = new WikiParser("block[link]")
      parse(parser, parser.StringExceptSPred("[]").run()) === "block"

      parser = new WikiParser("wit\\h\\quoted")
      parse(parser, parser.StringExceptSPred("qo").run()) === "withqu"

      parser = new WikiParser("Line String\n??")
      parse(parser, parser.LineStringExceptC('?').run()) === "Line String"

      parser = new WikiParser("No Match T_T")
      parse(parser, parser.LineStringExceptS("nOWAY!!").run()) === "No Match T_T"

      parser = new WikiParser("make one\n line")
      parse(parser, parser.LineString.run()) == "make one"
    }

    "parse normal paragraphs" in {
      parseAll("test paragraph 1.\ntest paragraph 2.\ntest paragraph 3.") === paraMaker (
        RS("test paragraph 1."), NA.BR,
        RS("test paragraph 2."), NA.BR,
        RS("test paragraph 3.")
      )
    }

    "parse One-Liners" in {
      var parser = new WikiParser("##Comment ##Check")
      parse(parser, parser.Comment.run()) === NA.Comment("Comment ##Check")
      parseAll("test ##0000\n##comment\n ##COMM\n#end") === paraMaker(
        RS("test ##0000"), NA.BR,
        NA.Comment("comment"), NA.BR,
        RS(" ##COMM"), NA.BR,
        RS("#end")
      )

      parser = new WikiParser("----------")
      parse(parser, parser.HR.run()) === NA.HR
      parseAll("HR\n----\n----------\n--\n-----------\n------------") === paraMaker(
        RS("HR"), NA.BR, NA.HR, NA.BR, NA.HR, NA.BR, RS("--"), NA.BR,
        paraMaker(NA.Strike(RS("")), NA.Strike(RS("")), RS("---")), NA.BR,
        paraMaker(NA.Strike(RS("")), NA.Strike(RS("")),NA.Strike(RS("")))
      )

      parseAll("== {{{ test }}} ==") === NA.RawHeadings(IS(" test ", false), 2)
      parseAll("== {{{ test }}} ==\n") ===
        paraMaker(NA.RawHeadings(IS(" test ", false), 2), NA.BR)
      parseAll("=== {{{ test }}} ==") ===
        paraMaker(RS("=== "), IS(" test ", false), RS(" =="))

      parseAll("= {{{ test }}} ==") ===
        paraMaker(RS("= "), IS(" test ", false), RS(" =="))

      parseAll("== {{{ test }}} ==5") ===
        paraMaker(RS("== "), IS(" test ", false), RS(" ==5"))
    }

    "parse Basic Blocks" in {
      var parser = new WikiParser("--strike--")
      parse(parser, parser.StrikeMinus.run()) === NA.Strike(RS("strike"))

      // failsafe
      parser = new WikiParser(" -- ")
      parse(parser, parser.LineTerm.run()) == RS(" -- ")

      parser = new WikiParser("block -- strike -- and ^^sup^^")
      parse(parser, parser.LineTerm.run()) ===
        paraMaker(
          RS("block "),
          NA.Strike(RS(" strike ")),
          RS(" and "),
          NA.Sup(RS("sup"))
        )

      parser = new WikiParser("''' bold in '' italic in __underline__'' '''")
      parse(parser, parser.LineTerm.run()) ===
        NA.Bold(
          paraMaker(
            RS(" bold in "),
            NA.Italic(
              paraMaker(
                RS(" italic in "),
                NA.Underline(RS("underline"))
              )
            ),
            RS(" ")
          )
        )
    }

    "parse Curly Brace - RawBlock" in {
      var parser = new WikiParser("{{{{\\{}}}}}")
      parse(parser, parser.RawBlock.run()) === IS("{\\{}}", isMultiLine=false)
      parseAll("{{{{\\{}}}}}") === IS("{\\{}}", false)

      parser = new WikiParser("block{\\{{{{ {\\{{te\nst}}} }}}")
      parse(parser, parser.LineTerm.run()) === paraMaker(RS("block{{"), IS(" {\\{{te\nst}}} ", false))
      parseAll("block{\\{{{{ {\\{{te\nst}}} }}}") === paraMaker(RS("block{{"), IS(" {\\{{te\nst}}} ", false))

      parseAll("{{{  \n this is Multi Liners\n}}}") ===
        IS(" this is Multi Liners", isMultiLine=true)
    }

    "parse Curly Brace - SpanBlock" in {
      var parser = new WikiParser("{{{+3 Plus3}}}")
      parse(parser, parser.SpanBlock.run()) === NA.SizeBlock(RS("Plus3"), 3)
      parseAll("{{{+3 Plus3}}}") === NA.SizeBlock(RS("Plus3"), 3)

      parser = new WikiParser("{{{#red red color}}}")
      parse(parser, parser.SpanBlock.run()) === NA.ColorBlock(RS("red color"), "red")
      parseAll("{{{#red red color}}}") === NA.ColorBlock(RS("red color"), "red")

      parser = new WikiParser("{{{#cafea1 cafe alpha}}}")
      parse(parser, parser.SpanBlock.run()) === NA.ColorBlock(RS("cafe alpha"), "#cafea1")
      parseAll("{{{#cafea1 cafe alpha}}}") === NA.ColorBlock(RS("cafe alpha"), "#cafea1")

      parser = new WikiParser("{{{#F14 Tomcat}}}")
      parse(parser, parser.SpanBlock.run()) === NA.ColorBlock(RS("Tomcat"), "#F14")
      parseAll("{{{#F14 Tomcat}}}") === NA.ColorBlock(RS("Tomcat"), "#F14")
    }

    "parse Curly Brace - Special Blocks" in {
      val demoTeul = """{{{#!wiki style="border:1px solid gray;border-top:5px solid orange;padding:12px"
                       |{{{+1 Plus Block! }}}[br][br]this is String}}}""".stripMargin
      val demoTeulParsed = NA.WikiBlock(
        "border:1px solid gray;border-top:5px solid orange;padding:12px",
        paraMaker(
          NA.SizeBlock(RS("Plus Block! "), 1),
          NA.BR, NA.BR, RS("this is String")
        )
      )
      var parser = new WikiParser(demoTeul)
      parse(parser, parser.WikiBlock.run()) === demoTeulParsed
      parseAll(demoTeul) === demoTeulParsed

      val demoSyntax =
        """{{{#!syntax scala
          |  def Redirect = rule { ("#redirect" | "#넘겨주기") ~ WL ~ LinkPath ~> NA.Redirect }
          |  def Comment = rule { "##" ~ LineString ~> NA.Comment }
          |  def HR = rule { (4 to 10).times(ch('-')) ~ &(NewLine | EOI) ~ push(NA.HR) }
          |}}}""".stripMargin
      val demoSyntaxParsed = NA.SyntaxBlock(
        "scala",
        """  def Redirect = rule { ("#redirect" | "#넘겨주기") ~ WL ~ LinkPath ~> NA.Redirect }
          |  def Comment = rule { "##" ~ LineString ~> NA.Comment }
          |  def HR = rule { (4 to 10).times(ch('-')) ~ &(NewLine | EOI) ~ push(NA.HR) }""".stripMargin
      )
      parser = new WikiParser(demoSyntax)
      parse(parser, parser.SyntaxBlock.run()) === demoSyntaxParsed
      parseAll(demoSyntax) === demoSyntaxParsed
    }

    "parse Links" in {
      var parser = new WikiParser("[[Simple Link]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.NormalHref("Simple Link"), None)
      parseAll("[[Simple Link]]") === NA.DocLink(NA.NormalHref("Simple Link"), None)

      parser = new WikiParser("[[Basic|Link]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.NormalHref("Basic"), Some(RS("Link")))
      parseAll("[[Basic|Link]]") === NA.DocLink(NA.NormalHref("Basic"), Some(RS("Link")))

      parser = new WikiParser("[[Non Basic\\|Link]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.NormalHref("Non Basic|Link"), None)
      parseAll("[[Non Basic\\|Link]]") === NA.DocLink(NA.NormalHref("Non Basic|Link"), None)

      parser = new WikiParser("[[Basic Anchor#s-1.5.1]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.ParaHref("Basic Anchor", Vector[Int](1,5,1)), None)

      parser = new WikiParser("[[Basic#anchor?]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.AnchorHref("Basic", "anchor?"), None)

      parseAll("More is Better: [[Li\\]]nk#s-11.3|\\|\\]][[#s-anchor|in Link]]]]\n") ===
        paraMaker(
          RS("More is Better: "),
          NA.DocLink(
            NA.ParaHref("Li]]nk", Vector[Int](11, 3)),
            Some(paraMaker(
              RS("|]]"),
              NA.DocLink(
                NA.SelfAnchorHref("s-anchor"),
                Some(RS("in Link"))
              )
            ))
          ),
          NA.BR
        )

      parser = new WikiParser("[[../|Upper]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.SuperDocHref, Some(RS("Upper")))

      parser = new WikiParser("[[../Fake]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.NormalHref("../Fake"), None)

      parser = new WikiParser("[[/Get Lower|Lower]]")
      parse(parser, parser.DocLink.run()) === NA.DocLink(NA.ChildDocHref(NA.NormalHref("Get Lower")), Some(RS("Lower")))

      parser = new WikiParser("[[https://www.example.com|example!!]]")
      parse(parser, parser.DocLink.run()) ===
        NA.DocLink(NA.ExternalHref("https://www.example.com"), Some(RS("example!!")))

      parser = new WikiParser("[[파일:some_file.jpg|width=30&height=10]]")
      parse(parser, parser.Link.run()) ===
        NA.FileLink("some_file.jpg", Map("width" -> "30", "height" -> "10"))
      parseAll("[[파일:some_file.jpg|width=30&height=10]]") ===
        NA.FileLink("some_file.jpg", Map("width" -> "30", "height" -> "10"))
    }

    "parse Macros" in {
      parseAll("[각주]  \n\n각주[br]테스트") === paraMaker(
        NA.FootNoteList, NA.BR, NA.BR, paraMaker(
          RS("각주"), NA.BR, RS("테스트")
        )
      )

      val parser = new WikiParser("[youtube(woei2928fa, width=640, height=130)]")
      parse(parser, parser.YoutubeLink.run()) === NA.YoutubeLink(
        "woei2928fa", Map("width" -> "640", "height" -> "130")
      )
      parseAll("[youtube(woei2928fa,width=640,height=130)]") === NA.YoutubeLink(
        "woei2928fa", Map("width" -> "640", "height" -> "130")
      )

      parseAll("[anchor(test)]") === NA.Anchor("test")
      parseAll("[include(틀:테스트,link=http://example.com)]") === NA.Include(
        "틀:테스트", Map("link" -> "http://example.com")
      )
    }

    "parse FootNote" in {
      var parser = new WikiParser("[* Simple '''FootNote''']")
      parse(parser, parser.FootNote.run()) ===
        NA.FootNote(paraMaker(RS("Simple "), NA.Bold(RS("FootNote"))), None)
      parseAll("[* Simple '''FootNote''']") ===
        NA.FootNote(paraMaker(RS("Simple "), NA.Bold(RS("FootNote"))), None)

      parser = new WikiParser("[*테스트 not simple --FootNote--]")
      parse(parser, parser.FootNote.run()) ===
        NA.FootNote(paraMaker(RS("not simple "), NA.Strike(RS("FootNote"))), Some("테스트"))
      parseAll("[*테스트 not simple --FootNote--]") ===
        NA.FootNote(paraMaker(RS("not simple "), NA.Strike(RS("FootNote"))), Some("테스트"))

      parseAll("[* 각주 in [* 각주]]") ===
        NA.FootNote(paraMaker(RS("각주 in "), NA.FootNote(RS("각주"), None)), None)
    }

    "parse BlockQuote" in {
      parseAll(">default BlockQuote") ===
        NA.BlockQuote(RS("default BlockQuote"))

      parseAll(">multiple\n>and> Multiple\n>>realMultiple") ===
        NA.BlockQuote(paraMaker(
          RS("multiple"), NA.BR,
          RS("and> Multiple"), NA.BR,
          NA.BlockQuote(RS("realMultiple"))
        ))
    }

    "parse TableCSS" in {
      var parser = new WikiParser("<table bordercolor=#FFEECC>")
      parse(parser, parser.TableCSS.run()) === NA.BorderColor("#FFEECC", forTable = true)

      parser = new WikiParser("<TableBgCOlor=Ruby>")
      parse(parser, parser.TableCSS.run()) === NA.BgColor("Ruby", forTable=true)

      parser = new WikiParser("<table  align=\"Right\">")
      parse(parser, parser.TableCSS.run()) === NA.Align(NA.AlignRightBottom, forTable=true)

      parser = new WikiParser("<Table width=\'252px\'>")
      parse(parser, parser.TableCSS.run()) === NA.Width("252px", forTable=true)

      parser = new WikiParser("<-5233>")
      parse(parser, parser.TableCSS.run()) === NA.ColSpan(5233)

      parser = new WikiParser("<|3242>")
      parse(parser, parser.TableCSS.run()) === NA.RowSpan(3242, NA.AlignCenter)

      parser = new WikiParser("<^|3>")
      parse(parser, parser.TableCSS.run()) === NA.RowSpan(3, NA.AlignLeftTop)

      parser = new WikiParser("<:>")
      parse(parser, parser.TableCSS.run()) === NA.Align(NA.AlignCenter, forTable=false)

      parser = new WikiParser("<width=303px>")
      parse(parser, parser.TableCSS.run()) === NA.Width("303px", forTable=false)

      parser = new WikiParser("<height=\"2em\">")
      parse(parser, parser.TableCSS.run()) === NA.Height("2em", forTable=false)

      parser = new WikiParser("<#BAB0BA>")
      parse(parser, parser.TableCSS.run()) === NA.BgColor("#BAB0BA", forTable=false)

      parser = new WikiParser("<WhiteBalance>")
      parse(parser, parser.TableCSS.run()) === NA.BgColor("WhiteBalance", forTable=false)
    }

    "parse Basic Table" in {
      var parser = new WikiParser("||Test||")
      parse(parser, parser.TD.run()) === NA.TD(RS("Test"), List[NA.TableStyle]())

      parser = new WikiParser("||Test||")
      println(parse(parser, parser.TDWithCaption.run()))

      parser = new WikiParser("||Test||")
      parse(parser, parser.TR.run()) ===
        NA.TR(List(NA.TD(RS("Test"), List[NA.TableStyle]())), List[NA.TableStyle]())

      parser = new WikiParser("||Test||")
      println(parse(parser, parser.TRWithCaption.run()))

      parser = new WikiParser("||Test||")
      parse(parser, parser.Table.run()) ===
        NA.TableWrapper(NA.Table(
          List(NA.TR(List(NA.TD(RS("Test"), List[NA.TableStyle]())), List[NA.TableStyle]())),
          List[NA.TableStyle]()),
          None)
    }

    "parse from file - Indent / Lists" in {
      1 === 1
    }
  }

  "HTMLRenderer" should {
    "render Link" in {
      renderAll("test", "[[Simple Link]]") === """<a href="entry://Simple Link">Simple Link</a>"""
    }

    "render NamuMark Original Document file" in {
      val namuHelpTxt = Source.fromFile("src/test/namu_help.txt").mkString
      renderAll("namu_help", namuHelpTxt).isInstanceOf[String] === true
    }
  }

  private def renderAll(title: String, markText: String): String = {
    val parser = new WikiParser(markText)
    val ast = parse(parser, parser.NamuMarkRule.run())
    val renderer = new HTMLRenderer
    val result = new ASTPostProcessor(title).postProcessAST(ast.asInstanceOf[NM])
    renderer.mainBody(result)
  }

  private def parseAll(markText: String): Any = {
    val parser = new WikiParser(markText)
    parse(parser, parser.NamuMarkRule.run())
  }

  private def parse(parser: WikiParser, parseResult: Try[Any]): Any = {
    parseResult match {
      case Success(result)        => result
      case Failure(e: ParseError) => sys.error(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e)             => throw e
    }
  }

  private def paraMaker(marks: NM*): NA.Paragraph = new PG(marks.toVector)
}