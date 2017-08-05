package net.kinetc.biryo

import net.kinetc.biryo.NamuAST._

object HTMLRenderer {
  val footnoteListClass = "footnote-list"
  val exLinkClass = "link-external"
  val ctClass = "content-table"
  val ctBeforeClass= "content-table-before"
  val ctIndentClass = "ct-indent"
  val ctItemClass = "ct-item"
  val indentClass = "indent"

  def c(className: String) = "class=\"" + className + "\""
}

/**
  * Created by KINETC on 2017-07-28.
  */
class HTMLRenderer {
  import HTMLRenderer._

  var footnotes: List[FootNote] = List()
  var headings: List[Headings] = List()

  // TODO : [각주]
  def generateHTML(title: String, mark: NamuMark): String = {
    mark.dfs(lister)

    // head + title + main paragraph + footnote list
    """<head>
      |<link rel="stylesheet" type="text/css" href="biryo.css" />
      |</head>
      |<body>""".stripMargin +
    s"<h1>$title</h1><hr>" +
    mark.dfsMap(renderMapper).mkString.replace("\n", "<br>") +
    s"<div ${c(footnoteListClass)}>" +
    footnotes.reverse.map(_.dfsMap(footNoteReverser).mkString.replace("\n", "<br>")).mkString +
    "</div></body>"
  }

  def lister(mark: NamuMark): Unit = {
    mark match {
      case h @ Headings(_, _) => headings ::= h
      case f @ FootNote(_, _) => footnotes ::= f
      case _ => ()
    }
  }

  def renderMapper: NamuMap = {
      case RawString(s) => RawString(escapeHTML(s))
      case InlineString(s) => InlineString(escapeHTML(s))
      case HTMLString(s) => HTMLString(deleteExternalTag(s))
      case TableOfContents => HTMLString(headingsRenderer(headings.reverse))
      case DocLink(href: ExternalHref, alias) => HTMLString(externalLinkRenderer(href, alias))
    }

  // can override this
  protected def headingsRenderer(headList: List[Headings]): String = {
    s"<div ${c(ctClass)}>" +
    s"<p ${c(ctBeforeClass)}>목차</p>" +
    headList.map(headingItemRenderer).mkString
    "</div>"
  }

  protected def headingItemRenderer(head: Headings): String = {
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
      case Some(nm) => s"<a ${c(exLinkClass)} href=${q}entry://${href.value}$q>${nm.mkString}</a>"
      case None => s"<a ${c(exLinkClass)} href=${q}entry://${href.value}$q>외부링크</a>"
    }
  }

  // ------ PRIVATE UTILITY FUNCTIONS

  private def footNoteReverser: NamuMap = {
    case RawString(s) => RawString(escapeHTML(s))
    case InlineString(s) => InlineString(escapeHTML(s))
    case HTMLString(s) => HTMLString(deleteExternalTag(s))
    case FootNote(v, n) => ReverseFootNote(v, n)
  }

  private def deleteExternalTag(s: String): String = {
    s.replaceAll("<embed>.*</embed>|<iframe>.*</iframe>", "").replaceAll("href=\"http", "href=\"entry://")
  }

  private def escapeHTML(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
  }

  // Renderer Only AST Node

  case class ReverseFootNote(value: NamuMark, noteStr: Option[String]) extends HasNamu {
    override def mkString = noteStr match {
      case Some(s) => s"<a name=$q$s$q></a><a href=${q}entry://#r$s$q>[$s]</a> ${value.mkString}<br>"
      case None => s"<a name=${q}WTF$q></a><a href=${q}entry://#rWTF$q>[*]</a> ${value.mkString}<br>"
    }
    def constructor(nm: NamuMark) = ReverseFootNote(nm, noteStr)
  }
}
