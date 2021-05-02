package net.kinetc.biryo.renderer

import net.kinetc.biryo.parser.NamuAST._

/** Created by KINETC on 2017-08-15.
  */
class FrameRenderer(private val katex: KatexRenderer)
    extends HTMLRenderer(katex) {

  import HTMLRenderer._

  override def generateHTML(title: String, mark: NamuMark): String = {
    mark.preTrav(lister)

    val mainParagraph =
      mark
        .postMap(renderMapper)
        .postMap(frameFootnoteRenderer)
        .mkString
        .replace("\n", "<br>")

    mainParagraph + footnotesRenderer(footnotes)
  }

  protected override def footnotesRenderer(fns: List[FootNote]): String = {
    if (fns.isEmpty)
      ""
    else
      s"<div ${c(footnoteListClass)}>" +
        fns.reverse
          .map(f =>
            FrameReverseFootNote(f.value, f.noteStr)
              .postMap(renderMapper)
              .preMap(frameFootnoteRenderer)
              .mkString
              .replace("\n", "<br>")
          )
          .mkString + "</div>"
  }

  // remove Doctype and Include
  protected def frameFootnoteRenderer: NamuMap = {
    case FootNote(v, n) => FrameFootnote(v, n)
    case Include(_, _)  => HTMLString("")
    case DocType(_)     => HTMLString("")
  }

  case class FrameFootnote(value: NamuMark, noteStr: Option[String])
      extends HasNamu {
    override def mkString = noteStr match {
      case Some(s) =>
        s"<a name=${toQ(s"fr$s")}></a><a href=${toQ(s"entry://#f$s")}>[$s]</a>"
      case None =>
        s"<a name=${toQ("frWTF")}></a><a href=${toQ("entry://#fWTF")}>[*]</a>"
    }

    def constructor(nm: NamuMark) = FootNote(nm, noteStr)
  }

  case class FrameReverseFootNote(value: NamuMark, noteStr: Option[String])
      extends HasNamu {
    override def mkString = {
      val content = noteStr match {
        case Some(s) =>
          s"<a name=${toQ(s"f$s")}></a><a href=${toQ(s"entry://#fr$s")}>[$s]</a> ${value.mkString}"
        case None =>
          s"<a name=${toQ("fWTF")}></a><a href=${toQ("entry://#frWTF")}>[*]</a> ${value.mkString}"
      }
      s"<div ${c(footnoteClass)}>$content</div>"
    }

    def constructor(nm: NamuMark) = ReverseFootNote(nm, noteStr)
  }

}
