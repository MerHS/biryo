package net.kinetc.biryo.renderer

import net.kinetc.biryo.parser.NamuAST

object HTMLRenderer {
  val foldingClass = "fold"
  val footnoteListClass = "fn-list"
  val footnoteClass = "fn-item"
  val exLinkClass = "lnk-ext"
  val exLinkHeaderClass = "lnk-ext-hd"
  val ctClass = "ct-tbl"
  val ctBeforeClass= "ct-tbl-bfr"
  val ctIndentClass = "ct-ind"
  val ctItemClass = "ct-item"
  val indentClass = "ind"
  val wordBoxClass = "wd-box"
  val docTypeClass = "dtype"
  val tableDivClass = "tbl-wrp"
  val tableClass = "tbl-top"
  val tocReference = "toc"
  val blurClass = "blur"

  def c(className: String) = "class=\"" + className + "\""

  def escapeURL(s: String): String = {
    if (s != null)
      s.replaceAll("\\\\", "%5C").replaceAll("#", "%23").replace("?", "%3F").replace("&", "%26")
    else
      ""
  }

  def escapeHTML(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
  }

  def escapeJSStr(s: String): String = {
    s.replaceAll(raw"\\", raw"\\\\").replaceAll("'", raw"\\'")
  }

  def escapeHTMLStr(s: String): String = {
    s.replaceAll("\\p{Punct}", "_p_")
  }

  def escapePathStr(s: String): String = {
    s.replaceAll("\\\\", "_b_").replaceAll("\\?", "_q_")
      .replaceAll("/", "_s_").replaceAll("\\*", "_s_")
      .replaceAll("&", "_a_").replaceAll("\"", "_d_")
      .replaceAll(":", "_c_")
  }

  def deleteExternalTag(s: String): String = {
    s.replaceAll("<embed(.|\\v)*</embed>|<iframe(.|\\v)*</iframe>", "")
      .replaceAll("href=\"http://", "href=\"entry://")
      .replaceAll("href=\"/w/", "href=\"entry://")
  }

  var inlineStyle = ""
  var useInlineCSS = false
}

/**
  * Created by KINETC on 2017-07-28.
  */
class HTMLRenderer(private val katex: KatexRenderer) {
  import HTMLRenderer._
  import NamuAST._

  protected var footnotes: List[FootNote] = List()
  protected var footnoteListLoc: List[Int] = List()
  protected var fnLoc = 0
  protected var headings: List[Headings] = List()
  protected var includes: List[(Include, String)] = List()
  protected var hasMathBlock: Boolean = false
  protected var isRedirect: Boolean = false
  protected var headListExists: Boolean = false

  def generateHTML(title: String, mark: NamuMark): String = {
    mark.preTrav(lister)

    footnotes = footnotes.reverse
    footnoteListLoc = footnoteListLoc.reverse

    val mainParagraph = mainBody(mark)

    // head + title + main paragraph + footnote list
    if (isRedirect) {
      mainParagraph
    } else {
      headRenderer() +
        "<body>" +
        """<a name="top"></a>""" +
        s"<h1 ${c("title")}>$title</h1>" +
        mainParagraph +
        """<div id="doctype-list"></div>""" +
        footnotesRenderer(footnotes) +
        s"<br><a href=${toQ(s"source://$title")}>[source]</a>" + // for debug
        "<a name=\"bottom\"></a>" +
        "</body>" +
        includeScriptListRenderer(includes) +
        """<script src="after.js"></script>""" +
        (if (hasMathBlock) """<script src="mathafter.js"></script>""" else "")
    }
  }

  def mainBody(mark: NamuMark) =
    mark.postMap(renderMapper).mkString.replace("\n", "<br>")

  def lister(mark: NamuMark): Unit = {
    mark match {
      case h @ Headings(_, _) => headings ::= h
      case f @ FootNote(_, _) => fnLoc += 1; footnotes ::= f
      case FootNoteList => footnoteListLoc ::= fnLoc; fnLoc = 0
      case Redirect(_) => isRedirect = true
      case _ => ()
    }
  }

  def renderMapper: NamuMap = {
      case RawString(s) => RawString(escapeHTML(s))
      case DocLink(href: ExternalHref, alias) => HTMLString(externalLinkRenderer(href, alias))
      case InlineString(s, i) => InlineString(escapeHTML(s), i)
      case HTMLString(s) => HTMLString(deleteExternalTag(s))
      case Include("틀:-", _) =>
        HTMLString("<div style=\"clear:both\"></div>")
      case Include("틀:루비", args) =>
        val value = escapeHTML(args.getOrElse("글자", ""))
        val ruby = escapeHTML(args.getOrElse("루비", ""))
        HTMLString(s"<ruby><rb>$value</rb><rp>(</rp><rt>$ruby</rt><rp>)</rp></ruby>")
      case i @ Include(s, _) if !useInlineCSS && s.startsWith("틀:") =>
        // TODO: check it in premap (산사나무)
        val id = escapeHTMLStr(s.substring(2)).replaceAll(" ", "_") + '-' + includes.length
        includes = (i, id) :: includes
        HTMLString(s"<div id=${toQ("틀-" + id)}></div>")
      case FootNoteList =>
        footnoteListLoc match {
          case h :: t =>
            val retVal = HTMLString(footnotesRenderer(footnotes.take(h)))
            footnoteListLoc = t
            footnotes = footnotes.drop(h)
            retVal
          case _ =>
            val retVal = HTMLString(footnotesRenderer(footnotes))
            footnotes = List()
            retVal
        }
      case SyntaxBlock(l, v) => SyntaxBlock(l, escapeHTML(v))
      case TableOfContents => HTMLString(headingsRenderer(headings.reverse))
      case MathBlock(s) =>
        // hasMathBlock = true
        HTMLString(katex.renderToString(s))
  }

  def includeScriptListRenderer(incs: List[(Include, String)]): String = {
    incs.map(x => includeScriptRenderer(x._1 , x._2)).mkString
  }

  def includeScriptRenderer(inc: Include, id: String): String = {
    if (!inc.rawHref.startsWith("틀:"))
      return ""

    val args = for ((k, v) <- inc.args) yield s"'${escapeJSStr(k)}': '${escapeJSStr(v)}'"
    val replArgObj = if (args.isEmpty) "''" else "{" + args.mkString(",") + "}"

    val href = inc.rawHref.substring(2).replaceAll(" ", "_")

    val phref = escapePathStr(href)
    s"<script src=${toQ(s"틀-$phref.js")}></script><script>repl($replArgObj, '$id', x);x='';</script>"
  }

  // can override this
  protected def headRenderer(): String = {
    "<head>" + {
      if (useInlineCSS)
        s"<style>\n$inlineStyle\n</style>"
      else
        (
          if (hasMathBlock) """<link rel="stylesheet" type="text/css" href="katex.min.css" />""" else ""
        ) +
        s"""
          |<style>.$foldingClass dd { display: none; }</style>
          |<link rel="stylesheet" type="text/css" href="biryo.css" />
          |<script src="jquery.min.js"></script>
          |<script src="frame.js"></script>""".stripMargin
    } + "</head>"
  }

  protected def footnotesRenderer(fns: List[FootNote]): String = {
    if (fns.isEmpty)
      ""
    else
      s"<div ${c(footnoteListClass)}>" +
        fns.map(
          f => ReverseFootNote(f.value, f.noteStr).postMap(renderMapper)
            .mkString.replace("\n", "<br>")
        ).mkString + "</div>"
  }

  protected def headingsRenderer(headList: List[Headings]): String = {
    headListExists = true
    s"<a name=${toQ(tocReference)}></a><div ${c(ctClass)}>" +
    s"<p ${c(ctBeforeClass)}>목차</p>" +
    s"<div ${c(ctIndentClass)}>" +
    headList.map(headingItemRenderer).mkString +
    "</div></div>"
  }

  protected def headingItemRenderer(head: Headings): String = {
    // Remove Footnotes from the table of contents (e.g. 나비에-스토크스 방정식)
    val newHead = (head postMap {
      case NamuAST.FootNote(_, _) => NamuAST.RawString("")
    }).asInstanceOf[Headings]

    val (value, no) = (newHead.value, newHead.no)
    val hno = no.mkString(".")
    val itemStr = s"<div ${c(ctItemClass)}><a href=${toQ(s"entry://#s-$hno")}>$hno.</a> ${value.mkString}</div>"
    if (no.lengthCompare(1) > 0) {
      s"<div style=${toQ(s"padding-left:${20 * (no.length - 1)}px")}>$itemStr</div>"
    } else {
      itemStr
    }
  }

  protected def externalLinkRenderer(href: ExternalHref, alias: Option[NamuMark]): String = {
    alias match {
      case Some(nm) =>
        s"<a ${c(exLinkClass)} href=${toQ(s"entry://${href.value}")}><span ${c(exLinkHeaderClass)}>#</span>${nm.mkString}</a>"
      case None =>
        s"<a ${c(exLinkClass)} href=${toQ(s"entry://${href.value}")}><span ${c(exLinkHeaderClass)}>#</span>외부링크</a>"
    }
  }

  // Renderer Only AST Node

  case class ReverseFootNote(value: NamuMark, noteStr: Option[String]) extends HasNamu {
    override def mkString = {
      val content = noteStr match {
        case Some(s) => s"<a name=${toQ(s"fn-$s")}></a><a href=${toQ(s"entry://#rfn-$s")}>[$s]</a> ${value.mkString}"
        case None => s"<a name=${toQ("fn-WTF")}></a><a href=${toQ("entry://#rfn-WTF")}>[*]</a> ${value.mkString}"
      }
      s"<div ${c(footnoteClass)}>$content</div>"
    }
    def constructor(nm: NamuMark) = ReverseFootNote(nm, noteStr)
  }
}
