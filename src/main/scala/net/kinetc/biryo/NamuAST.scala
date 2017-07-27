package net.kinetc.biryo

import scala.collection.Seq

// TODO: instantiate NamuAST Applicative fmap function
object NamuAST {
  sealed trait NamuMark {
    def htmlString: String = ""
  }
  sealed trait HasNamu {
    val value: NamuMark
    def constructor: NamuMark => NamuMark
    def map(f: NamuMark => NamuMark): NamuMark = constructor(f(value))
  }
  case class Paragraph(valueSeq: Seq[NamuMark]) extends NamuMark {
    override def htmlString =
      valueSeq.foldLeft(new StringBuilder)((sb, nm) => sb.append(nm.htmlString)).toString
  }
  case class ParagraphBuilder(markList: Seq[NamuMark], sb: StringBuilder) extends NamuMark

  case class RawListObj(value: NamuMark, listType: ListType, indentSize: Int) extends NamuMark with HasNamu {
    override def constructor = RawListObj(_, listType, indentSize)
  }

  case class FootNote(value: NamuMark, noteStr: Option[String]) extends NamuMark with HasNamu {
    override def constructor = FootNote(_, noteStr)
  }

  case class AgeMacro(date: String) extends NamuMark

  case object DateMacro extends NamuMark
  case object FootNoteList extends NamuMark
  case object TableOfContents extends NamuMark
  case class Include(rawHref: String, args: Map[String, String]) extends NamuMark
  case class Anchor(anchor: String) extends NamuMark
  case class YoutubeLink(id: String, args: Map[String, String]) extends NamuMark

  case class FileLink(href: String, htmlOption: Map[String, String]) extends NamuMark
  case class DocType(docType: String) extends NamuMark
  case class DocLink(href: NamuHref, alias: Option[NamuMark]) extends NamuMark {
    override def htmlString = alias match {
      case Some(nm) => "<a href=\""+href.value+"\">"+nm.htmlString+"</a>"
      case None => "<a href=\""+href.value+"\">"+href.value+"</a>"
    }
  }

  case class SyntaxBlock(language: String, value: String) extends NamuMark
  case class WikiBlock(style: String, value: NamuMark) extends NamuMark
  case class StringBox(value: NamuMark) extends NamuMark with HasNamu {
    override def constructor = StringBox
  }
  case class SizeBlock(value: NamuMark, size: Int) extends NamuMark with HasNamu {
    override def htmlString = "<font size=\"+"+size+"\">"+value.htmlString+"</font>"
    override def constructor = SizeBlock(_, size)
  }
  case class ColorBlock(value: NamuMark, color: String) extends NamuMark with HasNamu {
    override def htmlString = "<font color=\""+color+"\">"+value.htmlString+"</font>"
    override def constructor = ColorBlock(_, color)
  }

  case class RawHeadings(value:NamuMark, size: Int) extends NamuMark with HasNamu {
    override def constructor = RawHeadings(_, size)
  }


  sealed trait SpanMark
  case class Strike(value: NamuMark) extends NamuMark with SpanMark with HasNamu {
    override def htmlString = "<del>"+value.htmlString+"</del>"
    override def constructor = Strike
  }
  case class Sup(value: NamuMark) extends NamuMark with SpanMark with HasNamu {
    override def htmlString = "<sup>"+value.htmlString+"</sup>"
    override def constructor = Sup
  }
  case class Sub(value: NamuMark) extends NamuMark with SpanMark with HasNamu {
    override def htmlString = "<sub>"+value.htmlString+"</sub>"
    override def constructor = Sub
  }
  case class Underline(value: NamuMark) extends NamuMark with SpanMark with HasNamu {
    override def htmlString = "<u>"+value.htmlString+"</u>"
    override def constructor = Underline
  }
  case class Bold(value: NamuMark) extends NamuMark with SpanMark with HasNamu {
    override def htmlString = "<b>"+value.htmlString+"</b>"
    override def constructor = Bold
  }
  case class Italic(value: NamuMark) extends NamuMark with SpanMark with HasNamu {
    override def htmlString = "<i>"+value.htmlString+"</i>"
    override def constructor = Italic
  }


  // #redirect: 한글 -> Redirect(NormalHref(한글))
  case class Redirect(href: NamuHref) extends NamuMark
  // ##Comment -> Comment("Comment")
  case class Comment(value: String) extends NamuMark
  // HTML Unescaped String {{{#!html ... }}}
  case class HTMLString(value: String) extends NamuMark {
    override def htmlString = value
  }
  // HTML Escaped Normal String
  case class RawString(value: String) extends NamuMark {
    override def htmlString = value
  }
  // Markup Ignored String {{{ ... }}}
  case class InlineString(value: String) extends NamuMark {
    override def htmlString = "<code>"+value+"</code>"
  }
  // [br] -> BR
  case object BR extends NamuMark {
    override def htmlString = "<br>"
  }
  // ---- ~ ---------- (4 to 10 times)
  case object HR extends NamuMark {
    override def htmlString = "<hr>"
  }


  sealed trait NamuHref {
    val value: String
  }
  // [[value]] => NormalHref("value")
  case class NormalHref(value: String) extends NamuHref
  // [[value#s-0.1.2]] => ParaHref("value", Seq[Int](0, 1, 2))
  case class ParaHref(value: String, paraNo: Seq[Int]) extends NamuHref
  // [[value#anchor]] => AnchorHref("value", "anchor")
  case class AnchorHref(value: String, anchor: String) extends NamuHref
  // [[http://example.com]] => ExternalHref("http://example.com")
  case class ExternalHref(value: String) extends NamuHref
  // [[#1.4.1]] => SelfParaHref(Seq[Int](1,4,1))
  case class SelfParaHref(paraNo: Seq[Int]) extends NamuHref {
    val value: String = "#" + paraNo.mkString(".")
  }
  // [[#anchor]] => SelfAnchorHref("anchor")
  case class SelfAnchorHref(anchor: String) extends NamuHref {
    val value: String = "#" + anchor
  }
  // [[../]] => SuperDocHref
  case object SuperDocHref extends NamuHref {
    val value: String = "../"
  }
  // [[/child]] => ChildDocHref("child")
  case class ChildDocHref(childHref: NamuHref) extends NamuHref {
    val value: String = "/child"
  }

  sealed trait ListType
  // * star -> <ul> ~~ </ul>
  case object Type_star extends ListType
  // 1.#42 -> <ol type="1" start="42"> ~~ </ol>
  case class Type_1(offset: Int = 1) extends ListType
  // i.#42 -> <ol type="i" start="42"> ~~ </ol>
  case class Type_i(offset: Int = 1) extends ListType
  // I.#42 -> <ol type="I" start="42"> ~~ </ol>
  case class Type_I(offset: Int = 1) extends ListType
  // a.#42 -> <ol type="a" start="42"> ~~ </ol>
  case class Type_a(offset: Int = 1) extends ListType
  // A.#42 -> <ol type="A" start="42"> ~~ </ol>
  case class Type_A(offset: Int = 1) extends ListType

  // Post Process Only AST Node
  case class Headings(value: NamuMark, no: Seq[Int]) extends NamuMark with HasNamu {
    override def constructor = Headings(_, no)
  }

  // 최대한 Paragraph라는 Seq[NamuMark] Wrapper를 줄이기
  private[biryo] def pbResolver(pb: ParagraphBuilder): NamuMark = {
    if (pb.markList.isEmpty) {
      RawString(pb.sb.toString)
    } else if (pb.sb.isEmpty) {
      if (pb.markList.length == 1)
        pb.markList.head
      else
        Paragraph(pb.markList)
    } else {
      Paragraph(pb.markList :+ RawString(pb.sb.toString))
    }
  }

  private[biryo] def pbMerger(pb: ParagraphBuilder, lineObj: NamuMark): ParagraphBuilder = {
    if (pb.sb.isEmpty) {
      pb.markList match {
        case (p: Paragraph) +: Seq() => ParagraphBuilder(p.valueSeq :+ lineObj, pb.sb)
        case _ => ParagraphBuilder(pb.markList :+ lineObj, pb.sb)
      }
    } else {
      ParagraphBuilder(
        pb.markList :+ RawString(pb.sb.toString) :+ lineObj,
        new StringBuilder
      )
    }
  }
}

