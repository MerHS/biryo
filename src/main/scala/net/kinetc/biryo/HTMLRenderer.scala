package net.kinetc.biryo

import net.kinetc.biryo.NamuAST._

object HTMLRenderer {
  val footnoteListClass = "footnote-list"
  val footnoteClass = "footnote-item"
  val exLinkClass = "link-external"
  val exLinkHeaderClass = "link-external-header"
  val ctClass = "content-table"
  val ctBeforeClass= "content-table-before"
  val ctIndentClass = "ct-indent"
  val ctItemClass = "ct-item"
  val indentClass = "indent"
  val wordBoxClass = "word-box"
  val docTypeClass = "doctype"
  val tableDivClass = "table-wrapper"
  val tableClass = "table-top"

  def c(className: String) = "class=\"" + className + "\""

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

  var inlineStyle = ""
  var useInlineCSS = false
}

/**
  * Created by KINETC on 2017-07-28.
  */
class HTMLRenderer {
  import HTMLRenderer._

  var footnotes: List[FootNote] = List()
  var footnoteListLoc: List[Int] = List()
  var fnLoc = 0
  var headings: List[Headings] = List()
  var includes: List[(Include, String)] = List()
  var isRedirect: Boolean = false
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
        s"<h1 ${c("title")}>$title</h1>" +
        mainParagraph +
        footnotesRenderer(footnotes) +
        footerRenderer(includes) +
        """<div id="category-list"></div>""" +
        s"<br><a href=${toQ(s"source://$title")}>[source]</a>" + // for debug
        "</body>" +
        """<script src="after.js"></script>"""
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
      case InlineString(s, i) => InlineString(escapeHTML(s), i)
      case SyntaxBlock(l, v) => SyntaxBlock(l, escapeHTML(v))
      case HTMLString(s) => HTMLString(deleteExternalTag(s))
      case TableOfContents => HTMLString(headingsRenderer(headings.reverse))
      case DocLink(href: ExternalHref, alias) => HTMLString(externalLinkRenderer(href, alias))
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
        // TODO: check it in premap (산사나무)
      case i @ Include(s, _) if !useInlineCSS && s.startsWith("틀:") =>

        var id = escapeHTMLStr(s.substring(2)).replaceAll(" ", "_") + '-' + includes.length
        includes ::= (i, id)
        HTMLString(s"<div id=${toQ("틀-" + id)}></div>")
  }

  def footerRenderer(incs: List[(Include, String)]): String = {
    incs.map(x => includeFooterRenderer(x._1 , x._2)).mkString
  }

  def includeFooterRenderer(inc: Include, id: String): String = {
    if (!inc.rawHref.startsWith("틀:"))
      return ""

    val href = inc.rawHref.substring(2).replaceAll(" ", "_")
    val args =
      for ((k, v) <- inc.args)
        yield s"'${escapeJSStr(k)}': '${escapeJSStr(v)}'"
    val replArgObj =
      if (args.isEmpty) "''" else "{" + args.mkString(",") + "}"
    val phref = escapePathStr(href)
    s"<script src=${toQ("frame/" + phref + ".js")}></script><script>repl($replArgObj, '$id', x);x='';</script>"
  }

  // can override this
  protected def headRenderer(): String = {
    "<head>" + {
      if (useInlineCSS)
        s"<style>\n$inlineStyle\n</style>"
      else
        """
          |<link rel="stylesheet" type="text/css" href="biryo.css" />
          |<script src="jquery.min.js"></script>
          |<script src="frame.js"></script>
          |""".stripMargin
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
    s"<a name=${toQ("headList")}></a><div ${c(ctClass)}>" +
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
    if (no.length > 1) {
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

  protected def deleteExternalTag(s: String): String = {
    s.replaceAll("<embed.*</embed>|<iframe.*</iframe>", "")
      .replaceAll("href=\"http", "href=\"entry://")
  }

  // Renderer Only AST Node

  case class ReverseFootNote(value: NamuMark, noteStr: Option[String]) extends HasNamu {
    override def mkString = {
      val content = noteStr match {
        case Some(s) => s"<a name=${toQ(s)}></a><a href=${toQ(s"entry://#r$s")}>[$s]</a> ${value.mkString}"
        case None => s"<a name=${toQ("WTF")}></a><a href=${toQ("entry://#rWTF")}>[*]</a> ${value.mkString}"
      }
      s"<div ${c(footnoteClass)}>$content</div>"
    }
    def constructor(nm: NamuMark) = ReverseFootNote(nm, noteStr)
  }
}
