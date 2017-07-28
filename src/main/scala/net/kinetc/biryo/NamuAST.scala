package net.kinetc.biryo

import scala.collection.Seq

// TODO: instantiate NamuAST Applicative fmap function
object NamuAST {
  type NamuMap = PartialFunction[NamuMark, NamuMark]
  // s"\"" -> s"$q"  (Build Error???)
  private val q = '"'

  sealed trait NamuMark {
    def mkString: String = ""

    /**
      * Depth-First Search Mapping
      * @param f: PartialFunction, don't need to apply it to child
      * @return default: this, or f.apply(this)
      */
    def dfsMap(f: NamuMap): NamuMark = if (f.isDefinedAt(this)) f(this) else this

    /**
      * Breath-First Search Mapping
      * @param f PartialFunction, You should apply it to child when `this` is Paragraph or DocLink
      * @return default: this, or f.apply(this)
      */
    def bfsMap(f: NamuMap): NamuMark = dfsMap(f)
  }

  sealed trait HasNamu extends NamuMark {
    val value: NamuMark
    def constructor(nm: NamuMark): NamuMark
    override def dfsMap(f: NamuMap): NamuMark = {
      val childMap = value.dfsMap(f)
      val newThis = constructor(childMap)
      if (f.isDefinedAt(newThis)) f(newThis) else newThis
    }
    override def bfsMap(f: NamuMap): NamuMark = {
      if (f.isDefinedAt(this)) {
        val newThis = f(this)
        newThis match {
          case t: HasNamu => constructor(t.value.bfsMap(f))
          case _ => newThis
        }
      } else {
        constructor(value.bfsMap(f))
      }
    }
  }

  sealed trait HasHref {
    val href: NamuHref
    def hrefConstructor(href: NamuHref): NamuMark
    def hrefMap(f: NamuHref => NamuHref): NamuMark = hrefConstructor(f(href))
  }


  case class Paragraph(valueSeq: Seq[NamuMark]) extends NamuMark {
    override def mkString =
      valueSeq.foldLeft(new StringBuilder)((sb, nm) => sb.append(nm.mkString)).toString
    override def dfsMap(f: NamuMap) = {
      val childMap = valueSeq.map(_.dfsMap(f))
      val newThis = Paragraph(childMap)
      if (f.isDefinedAt(newThis)) f(newThis) else newThis
    }
    override def bfsMap(f: NamuMap): NamuMark =
      if (f.isDefinedAt(this)) f(this) else Paragraph(valueSeq.map(_.bfsMap(f)))
  }

  case class ParagraphBuilder(markList: Seq[NamuMark], sb: StringBuilder) extends NamuMark

  case class ListObj(value: NamuMark, listType: ListType, indentSize: Int) extends HasNamu {
    def constructor(nm: NamuMark) = ListObj(nm, listType, indentSize)
  }

  case class FootNote(value: NamuMark, noteStr: Option[String]) extends HasNamu {
    override def mkString = noteStr match {
      case Some(s) => s"<a name=${q}r$s$q></a><a href=${q}entry://#$s$q>[$s]</a>"
      case None => s"<a name=${q}rWTF$q></a><a href=${q}entry://#WTF$q>[*]</a>"
    }
    def constructor(nm: NamuMark) = FootNote(nm, noteStr)
  }

  // TODO: Calculate This!
  case class AgeMacro(date: String) extends NamuMark {
    override def mkString = s"(${date}로부터 나이)"
  }
  case object DateMacro extends NamuMark {
    // do not calculate it for MDict
    override def mkString = "[현재 시간]"
  }
  // TODO: Render This From HTMLRenderer
  case object FootNoteList extends NamuMark {
    override def mkString = "[각주]"
  }
  // TODO: Render This From HTMLRenderer
  case object TableOfContents extends NamuMark {
    override def mkString = "[목차]"
  }
  // TODO: Should we render this??
  case class Include(rawHref: String, args: Map[String, String]) extends NamuMark {
    override def mkString = {
      if (args.isEmpty) {
        s"[include(<a href=${q}entry://$rawHref$q>$rawHref</a>)]"
      } else {
        val argString = args.mkString(", ").replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
        s"[include(<a href=${q}entry://$rawHref$q>$rawHref</a>), args:$argString)]"
      }
    }
  }
  case class Anchor(anchor: String) extends NamuMark {
    override def mkString = s"<a name=$q#$anchor$q></a>"
  }

  case class YoutubeLink(id: String, args: Map[String, String]) extends NamuMark {
    // Fallback to Link (For MDict)
    override def mkString = s"<a href=${q}https://www.youtube.com/watch?v=$id$q>[유튜브 링크]</a>"
  }

  // [[파일:$href|$htmlOption]]
  case class FileLink(href: String, htmlOption: Map[String, String]) extends NamuMark {
    // Fallback to Link (for Mdict)
    override def mkString = s"<a href=${q}entry://$href$q>[파일:$href]</a>"
  }
  // [[분류:$docType]]
  case class DocType(docType: String) extends NamuMark {
    override def mkString = s"<div style=${q}border:1px solid gray; padding:5px;$q>" +
      s"분류: <a href=${q}entry://분류:$docType$q>$docType</a></div>"
  }
  // [[$href|$alias]] -> href will be changed to NormalHref after the postprocessing
  case class DocLink(href: NamuHref, alias: Option[NamuMark]) extends HasNamu with HasHref {
    override def mkString = alias match {
      case Some(nm) => s"<a href=${q}entry://${href.value}$q>${nm.mkString}</a>"
      case None => s"<a href=${q}entry://${href.value}$q>${href.value}</a>"
    }
    val value: NamuMark = alias.orNull
    override def constructor(nm: NamuMark) = DocLink(href, Some(nm))
    override def dfsMap(f: NamuMap) = {
      val childMap = alias.map(_.dfsMap(f))
      val newThis = DocLink(href, alias.map(_.dfsMap(f)))
      if (f.isDefinedAt(newThis)) f(newThis) else newThis
    }

    override def bfsMap(f: NamuMap): NamuMark =
      if (f.isDefinedAt(this)) f(this) else DocLink(href, alias.map(_.bfsMap(f)))
    def hrefConstructor(href: NamuHref): NamuMark = DocLink(href, alias)
  }

  // {{{#!syntax $language $value}}}
  case class SyntaxBlock(language: String, value: String) extends NamuMark {
    override def mkString = s"<pre><code>$value</code></pre>"
  }
  // {{{$!wiki style="$style" $value}}}
  case class WikiBlock(style: String, value: NamuMark) extends NamuMark {
    override def mkString = s"<div style=$q$style$q>${value.mkString}</div>"
  }
  // {{|$value|}}
  case class StringBox(value: NamuMark) extends NamuMark with HasNamu {
    override def mkString = s"<table><tbody><td><tr><p>${value.mkString}</p></tr></td></tbody></table>"
    def constructor(nm: NamuMark) = StringBox(nm)
  }
  case class SizeBlock(value: NamuMark, size: Int) extends HasNamu {
    override def mkString = s"<font size=$q+$size$q>${value.mkString}</font>"
    def constructor(nm: NamuMark) = SizeBlock(nm, size)
  }
  case class ColorBlock(value: NamuMark, color: String) extends HasNamu {
    override def mkString = s"<font color=$q$color$q>${value.mkString}</font>"
    def constructor(nm: NamuMark) = ColorBlock(nm, color)
  }

  case class RawHeadings(value:NamuMark, size: Int) extends HasNamu {
    override def mkString = s"<h$size>${value.mkString}</h$size><hr>"
    def constructor(nm: NamuMark) = RawHeadings(nm, size)
  }
  // Post Process Only AST Node
  case class Headings(value: NamuMark, no: Seq[Int]) extends HasNamu {
    override def mkString = {
      val hsize = if (no.length <= 5) no.length + 1 else 6
      val hno = no.mkString(".")
      s"<h$hsize><a name=${q}s-$hno$q><font color=${q}blue$q>$hno.</font></a>${value.mkString}</h$hsize><hr>"
    }
    def constructor(nm: NamuMark) = Headings(nm, no)
  }


  sealed trait SpanMark
  case class Strike(value: NamuMark) extends HasNamu with SpanMark {
    override def mkString = s"<del>${value.mkString}</del>"
    def constructor(nm: NamuMark) = Strike(nm)
  }
  case class Sup(value: NamuMark) extends HasNamu with SpanMark {
    override def mkString = s"<sup>${value.mkString}</sup>"
    def constructor(nm: NamuMark) = Sup(nm)
  }
  case class Sub(value: NamuMark) extends HasNamu with SpanMark {
    override def mkString = s"<sub>${value.mkString}</sub>"
    def constructor(nm: NamuMark) = Sub(nm)
  }
  case class Underline(value: NamuMark) extends HasNamu with SpanMark {
    override def mkString = s"<u>${value.mkString}</u>"
    def constructor(nm: NamuMark) = Underline(nm)
  }
  case class Bold(value: NamuMark) extends HasNamu with SpanMark {
    override def mkString = s"<b>${value.mkString}</b>"
    def constructor(nm: NamuMark) = Bold(nm)
  }
  case class Italic(value: NamuMark) extends HasNamu with SpanMark {
    override def mkString = s"<i>${value.mkString}</i>"
    def constructor(nm: NamuMark) = Italic(nm)
  }


  // #redirect: 한글 -> Redirect(NormalHref(한글))
  case class Redirect(href: NamuHref) extends NamuMark with HasHref {
    override def mkString = s"<a href=$q${href.value}$q>리다이렉트:${href.value}</a>"
    override def hrefConstructor(href: NamuHref) = Redirect(href)
  }
  // ##Comment -> Comment("Comment")
  case class Comment(value: String) extends NamuMark
  // HTML Unescaped String {{{#!html ... }}}
  case class HTMLString(value: String) extends NamuMark {
    override def mkString = value
  }
  // HTML Escaped Normal String
  case class RawString(value: String) extends NamuMark {
    override def mkString = value
  }
  // Markup Ignored String {{{ ... }}}
  case class InlineString(value: String) extends NamuMark {
    override def mkString = s"<code>$value</code>"
  }
  // [br] -> BR
  case object BR extends NamuMark {
    override def mkString = "<br>"
  }
  // ---- ~ ---------- (4 to 10 times)
  case object HR extends NamuMark {
    override def mkString = "<hr>"
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
    val value: String = s"/${childHref.value}"
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

