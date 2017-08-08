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
}

/**
  * Created by KINETC on 2017-07-28.
  */
class HTMLRenderer {
  import HTMLRenderer._

  var footnotes: List[FootNote] = List()
  var headings: List[Headings] = List()
  protected var headListExists: Boolean = false

  // TODO : [각주] macro
  def generateHTML(title: String, mark: NamuMark): String = {
    mark.preTrav(lister)

    val mainParagraph = mainBody(mark)

    // head + title + main paragraph + footnote list
    """<head>
      |<link rel="stylesheet" type="text/css" href="biryo.css" />
      |</head>
      |<body>""".stripMargin +
    s"<h1 ${c("title")}>$title</h1>" +
    (if (headListExists) "" else "<a name=\"headList\"></a>") +
    mainParagraph +
    s"<div ${c(footnoteListClass)}>" +
    footnotes.reverse.map(
      f => ReverseFootNote(f.value, f.noteStr).postMap(renderMapper)
          .mkString.replace("\n", "<br>")
    ).mkString +
    "</div></body>"
  }

  def mainBody(mark: NamuMark) =
    mark.postMap(renderMapper).mkString.replace("\n", "<br>")

  def lister(mark: NamuMark): Unit = {
    mark match {
      case h @ Headings(_, _) => headings ::= h
      case f @ FootNote(_, _) => footnotes ::= f
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
      case Include("틀:루비", args) => {
        val value = escapeHTML(args.getOrElse("글자", ""))
        val ruby = escapeHTML(args.getOrElse("루비", ""))
        HTMLString(s"<ruby><rb>$value</rb><rp>(</rp><rt>$ruby</rt><rp>)</rp></ruby>")
      }

  }

  // can override this
  protected def headingsRenderer(headList: List[Headings]): String = {
    headListExists = true
    s"<a name=${q}headList$q></a><div ${c(ctClass)}>" +
    s"<p ${c(ctBeforeClass)}>목차</p>" +
    s"<div ${c(ctIndentClass)}>" +
    headList.map(headingItemRenderer).mkString +
    "</div></div>"
  }

  protected def headingItemRenderer(head: Headings): String = {
    // Remove Footnotes from the table of contents (e.g. 나비에-스토크스 방정식)
    head postMap {
      case NamuAST.FootNote(_, _) => NamuAST.RawString("")
    }
    val (value, no) = (head.value, head.no)
    val hno = no.mkString(".")
    val itemStr = s"<div ${c(ctItemClass)}><a href=${q}entry://#s-$hno$q>$hno.</a> ${value.mkString}</div>"
    if (no.length > 1) {
      s"<div style=${q}padding-left:${20 * (no.length - 1)}px$q>$itemStr</div>"
    } else {
      itemStr
    }
  }

  protected def externalLinkRenderer(href: ExternalHref, alias: Option[NamuMark]): String = {
    alias match {
      case Some(nm) =>
        s"<a ${c(exLinkClass)} href=${q}entry://${href.value}$q><span ${c(exLinkHeaderClass)}>#</span>${nm.mkString}</a>"
      case None =>
        s"<a ${c(exLinkClass)} href=${q}entry://${href.value}$q><span ${c(exLinkHeaderClass)}>#</span>외부링크</a>"
    }
  }

  // ------ PRIVATE UTILITY FUNCTIONS

  private def deleteExternalTag(s: String): String = {
    s.replaceAll("<embed.*</embed>|<iframe.*</iframe>", "")
      .replaceAll("href=\"http", "href=\"entry://")
  }

  private def escapeHTML(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
  }

  // Renderer Only AST Node

  case class ReverseFootNote(value: NamuMark, noteStr: Option[String]) extends HasNamu {
    override def mkString = {
      val content = noteStr match {
        case Some(s) => s"<a name=$q$s$q></a><a href=${q}entry://#r$s$q>[$s]</a> ${value.mkString}"
        case None => s"<a name=${q}WTF$q></a><a href=${q}entry://#rWTF$q>[*]</a> ${value.mkString}"
      }
      s"<div ${c(footnoteClass)}>$content</div>"
    }
    def constructor(nm: NamuMark) = ReverseFootNote(nm, noteStr)
  }
}
