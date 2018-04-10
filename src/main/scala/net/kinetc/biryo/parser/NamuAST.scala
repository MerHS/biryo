package net.kinetc.biryo.parser

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime}

import net.kinetc.biryo.renderer.HTMLRenderer._

import scala.collection.Seq
import scala.util.{Failure, Success, Try}

object NamuAST {
  type NamuMap = PartialFunction[NamuMark, NamuMark]

  def toQ(s: String) = '\"' + s + '\"'
  def toPx(value: String): String = Try(value.toDouble) match {
    case Success(_) => s"${value}px"
    case _ => value
  }

  trait NamuMark {
    def mkString: String = ""

    /**
      * Depth-First Post-Order Traversing
      */
    def postTrav(f: NamuMark => Unit): Unit = f(this)

    /**
      * Depth-First Pre-Order Traversing
      */
    def preTrav(f: NamuMark => Unit): Unit = f(this)

    /**
      * Depth-First Post-Order Node Mapping
      *
      * @param f: PartialFunction, don't need to apply it to child
      * @return default: this, or f.apply(this)
      */
    def postMap(f: NamuMap): NamuMark = if (f.isDefinedAt(this)) f(this) else this

    /**
      * Depth-First Pre-Order Node Mapping
      *
      * @param f PartialFunction, don't need to apply it to child
      * @return default: this, or f.apply(this)
      */
    def preMap(f: NamuMap): NamuMark = postMap(f)

    /**
      * Map f To Child (NFS Helper Function)
      */
    def map(f: NamuMap): NamuMark = this
  }

  /**
    * A Trait which has a single NamuMark value
    */
  trait HasNamu extends NamuMark {
    val value: NamuMark
    def constructor(nm: NamuMark): NamuMark

    override def postTrav(f: (NamuMark) => Unit): Unit = {
      value.postTrav(f); f(this)
    }

    override def preTrav(f: (NamuMark) => Unit): Unit = {
      f(this); value.preTrav(f)
    }

    override def postMap(f: NamuMap): NamuMark = {
      val childMap = value.postMap(f)
      val newThis = constructor(childMap)
      if (f.isDefinedAt(newThis)) f(newThis) else newThis
    }

    override def preMap(f: NamuMap): NamuMark = {
      val newThis = if (f.isDefinedAt(this)) f(this) else this
      newThis.map(f)
    }

    override def map(f: NamuMap): NamuMark =
      constructor(value.preMap(f))
  }

  implicit def trav2Option[K <: NamuMark](x: Traversable[K]): Option[K] = x.headOption

  implicit def trav2Seq[K <: NamuMark](x: Traversable[K]): Seq[K] = x.toSeq

  /**
    * A Class that has a derived type or sequence of NamuMark
    * @param valueSeq Traversable NamuMark Values
    * @tparam K Child of NamuMark
    * @tparam T can be Traversable implicitly (e.g. Option, Seq, ...)
    */
  abstract class HasNamuSeq[+K <: NamuMark, T](valueSeq: T)(
    implicit e: T => Traversable[K], revE: Traversable[K] => T) extends NamuMark {

    def constructorSeq(nm: T): HasNamuSeq[K, T]

    override def mkString =
      valueSeq.map(_.mkString).addString(new StringBuilder).toString

    override def postTrav(f: (NamuMark) => Unit): Unit = {
      valueSeq.foreach(_.postTrav(f)); f(this)
    }

    override def preTrav(f: (NamuMark) => Unit): Unit = {
      f(this); valueSeq.foreach(_.preTrav(f))
    }

    override def postMap(f: NamuMap) = {
      val childMap = valueSeq.map(_.postMap(f)).asInstanceOf[Traversable[K]]
      val newThis = constructorSeq(childMap)
      if (f.isDefinedAt(newThis)) f(newThis) else newThis
    }

    override def preMap(f: NamuMap): NamuMark = {
      val newThis = if (f.isDefinedAt(this)) f(this) else this
      newThis.map(f)
    }

    override def map(f: NamuMap): HasNamuSeq[K, T] = {
      val mapped = valueSeq.map(_.preMap(f)).asInstanceOf[Traversable[K]]
      constructorSeq(mapped)
    }
  }


  ////// ------ Href Traits ------- ///////


  trait HasHref {
    val href: NamuHref
    def hrefConstructor(href: NamuHref): NamuMark
    def hrefMap(f: NamuHref => NamuHref): NamuMark = hrefConstructor(f(href))
  }

  sealed trait NamuHref {
    val value: String
    val escapeValue = escapeURL(value)
  }
  // for ASTPostProcessors hrefProcessor
  // NormalHref("val#ue") => RawHref("val%23ue")
  case class RawHref(value: String, override val escapeValue: String) extends NamuHref

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
    val value: String = "#s-" + paraNo.mkString(".")
    override val escapeValue = value
  }

  // [[#anchor]] => SelfAnchorHref("anchor")
  case class SelfAnchorHref(anchor: String) extends NamuHref {
    val value: String = "#" + anchor
    override val escapeValue = "#" + escapeURL(anchor)
  }

  // [[../]] => SuperDocHref
  case object SuperDocHref extends NamuHref {
    val value: String = "../"
  }

  // [[/child]] => ChildDocHref("child")
  case class ChildDocHref(childHref: NamuHref) extends NamuHref {
    val value: String = s"/${childHref.value}"
  }



  ////// ------ Paragraph ------- ///////


  /**
    * List of NamuMark Objects (`Seq[NamuMark]` Wrapper)
    * @param valueSeq a sequence of NamuMark Objects
    */
  case class Paragraph(valueSeq: Seq[NamuMark]) extends HasNamuSeq[NamuMark, Seq[NamuMark]](valueSeq) {
    def constructorSeq(nm: Seq[NamuMark]) = Paragraph(nm)
  }

  case class ParagraphBuilder(markList: Seq[NamuMark], sb: StringBuilder) extends NamuMark


  ////// ------ Indent & Lists ------ //////


  case class ListObj(value: NamuMark, listType: ListType, indentSize: Int) extends HasNamu {
    def constructor(nm: NamuMark) = ListObj(nm, listType, indentSize)
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


  ////// ----- Indent ------ //////


  case class Indent(value: NamuMark, size: Int) extends HasNamu {
    override def mkString = s"<div ${c(indentClass)}>${value.mkString}</div>"

    override def constructor(nm: NamuMark) = Indent(nm, size)
  }


  ////// ------ Table Styles ------ //////


  sealed trait TableAlign
  case object AlignLeftTop extends TableAlign
  case object AlignCenter extends TableAlign
  case object AlignRightBottom extends TableAlign

  sealed trait TableStyle
  sealed trait TableAttr extends TableStyle {
    def attr: String
  }
  sealed trait TableCSS extends TableStyle {
    def style: String
  }
  sealed trait ForTable extends TableStyle {
    def forTable: Boolean
  }

  case class BgColor(value: String, forTable: Boolean) extends TableCSS with ForTable {
    def style: String = s"background-color:$value"
  }

  case class RowBgColor(value: String) extends TableCSS {
    def style: String = s"background-color:$value"
  }

  case class BorderColor(value: String, forTable: Boolean) extends TableCSS with ForTable {
    def style: String = s"border:2px solid $value"
  }

  case class Width(value: String, forTable: Boolean) extends TableCSS with ForTable {
    def style: String = s"width:${toPx(value)}"
  }

  case class Height(value: String, forTable: Boolean) extends TableCSS with ForTable {
    def style: String = s"height:${toPx(value)}"
  }

  case class Align(align: TableAlign, forTable: Boolean) extends TableCSS with ForTable {
    def style: String =
      if (forTable) {
        align match {
          case AlignCenter => "margin:0 auto"
          case AlignRightBottom => "margin-right:0px"
          case AlignLeftTop => "margin-left:0px" // default
        }
      } else {
        align match {
          case AlignCenter => "text-align:center"
          case AlignRightBottom => "text-align:right"
          case AlignLeftTop => "text-align:left" // default
        }
      }
  }

  case class RowSpan(value: Int, align: TableAlign) extends TableCSS with TableAttr {
    def style: String = align match {
      case AlignCenter => "" // default
      case AlignRightBottom => "vertical-align:bottom"
      case AlignLeftTop => "vertical-align:top"
    }
    def attr: String = s"rowspan=${toQ(value.toString)}"
  }

  case class ColSpan(value: Int) extends TableAttr {
    def attr: String = s"colspan=${toQ(value.toString)}"
  }


  ////// ------ Table ------ //////


  trait HasTableStyle {
    val styles: Seq[TableStyle]

    def attrStr: String = {
      val attributes = styles.filter(_.isInstanceOf[TableAttr]).map(_.asInstanceOf[TableAttr])
      if (attributes.nonEmpty) {
        attributes.map(_.attr).addString(new StringBuilder, " ").toString
      } else ""
    }

    def styleStr: String = {
      val inlineStyles = styles.filter(_.isInstanceOf[TableCSS]).map(_.asInstanceOf[TableCSS])
      if (inlineStyles.nonEmpty) {
        val sb = new StringBuilder("style=\"")
        inlineStyles.map(_.style).addString(sb, ";").append('\"').toString
      } else ""
    }
  }

  case class TableWrapper(value: Table, caption: Option[NamuMark])
    extends HasNamuSeq[NamuMark, Option[NamuMark]](caption) {
    def constructorSeq(nm: Option[NamuMark]) = TableWrapper(value, nm)

    override def mkString =
      if (caption.isDefined) {
        s"<div ${c(tableDivClass)}>${value.mkStringWCaption(caption.get)}</div>"
      } else {
        s"<div ${c(tableDivClass)}>${value.mkString}</div>"
      }

    override def postTrav(f: (NamuMark) => Unit): Unit = {
      caption.foreach(_.postTrav(f))
      value.postTrav(f)
      f(this)
    }

    override def preTrav(f: (NamuMark) => Unit): Unit = {
      f(this)
      caption.foreach(_.preTrav(f))
      value.preTrav(f)
    }

    override def postMap(f: NamuMap) = {
      val captionMap = caption.map(_.postMap(f))
      val valueMap = value.postMap(f)
      val newThis = valueMap match {
        case t: Table => TableWrapper(t, captionMap)
        case _ => valueMap
      }
      if (f.isDefinedAt(newThis)) f(newThis) else newThis
    }

    override def map(f: NamuMap): HasNamuSeq[NamuMark, Option[NamuMark]] = {
      val captionMap = caption.map(_.preMap(f))
      val valueMap = value.preMap(f)
      valueMap match {
        case t: Table => TableWrapper(t, captionMap)
        case _ => OptionWrapper(Some(valueMap))
      }
    }
  }

  case class OptionWrapper(valueSeq: Option[NamuMark])
    extends HasNamuSeq[NamuMark, Option[NamuMark]](valueSeq) {
    override def constructorSeq(nm: Option[NamuMark]) = OptionWrapper(nm)
  }

  case class Table(valueSeq: Seq[TR], styles: Seq[TableStyle])
    extends HasNamuSeq[TR, Seq[TR]](valueSeq) with HasTableStyle {

    def constructorSeq(nm: Seq[TR]) = Table(nm, styles)
    override def mkString = s"<table $attrStr $styleStr ${c(tableClass)}><tbody>$trString</tbody></table>"

    def mkStringWCaption(caption: NamuMark): String =
      s"<table $attrStr $styleStr ${c(tableClass)}><caption>${caption.mkString}</caption><tbody>$trString</tbody></table>"

    def trString: String =
      valueSeq.map(_.mkString).addString(new StringBuilder).toString
  }

  case class TR(valueSeq: Seq[TD], styles: Seq[TableStyle])
    extends HasNamuSeq[TD, Seq[TD]](valueSeq) with HasTableStyle {

    def constructorSeq(nm: Seq[TD]) = TR(nm, styles)
    override def mkString = s"<tr $attrStr $styleStr>$tdString</tr>"

    def tdString = valueSeq.map(_.mkString).addString(new StringBuilder).toString
  }

  case class TD(value: NamuMark, styles: Seq[TableStyle])
    extends HasNamu with HasTableStyle {

    def constructor(nm: NamuMark) = TD(nm, styles)

    override def mkString = s"<td $attrStr $styleStr>${value.mkString}</td>"
  }


  ////// ------- BlockQuote ------ //////


  case class BlockQuote(value: NamuMark) extends HasNamu {
    override def mkString = s"<blockquote><div ${c(indentClass)}>${value.mkString}</div></blockquote>"
    def constructor(nm: NamuMark) = BlockQuote(nm)
  }


  ////// ------ Single Bracket FootNote / Macros ------ //////


  case class Anchor(anchor: String) extends NamuMark {
    override def mkString = s"<a name=${toQ(anchor)}></a>"
  }

  case class FootNote(value: NamuMark, noteStr: Option[String]) extends HasNamu {
    override def mkString = noteStr match {
      case Some(s) => s"<a name=${toQ(s"rfn-$s")}></a><a href=${toQ(s"entry://#fn-$s")}>[$s]</a>"
      case None => s"<a name=${toQ("rfn-WTF")}></a><a href=${toQ("entry://#fn-WTF")}>[*]</a>"
    }
    def constructor(nm: NamuMark) = FootNote(nm, noteStr)
  }

  case class LinkOnlyFN(noteStr: String) extends NamuMark {
    override def mkString = s"<a href=${toQ(s"entry://#fn-$noteStr")}>[$noteStr]</a>"
  }

  case class AgeMacro(dateText: String) extends NamuMark {
    override def mkString =
      Try(LocalDate.parse(dateText)) match {
        case Success(date) => ChronoUnit.YEARS.between(date, currTime).toString
        case Failure(_) => s"[age($dateText)]"
    }
  }

  case class PageCount(namespace: String) extends NamuMark {
    override def mkString = s"<span ${c(s"pc-$namespaceClass")}>[$namespace 개수]</span>"
    val namespaceClassMap = Map(
      "문서" -> "dc", "파일" -> "fl", "분류" -> "dt",
      "틀" -> "fr", "나무위키" -> "nm", "사용자" -> "us"
    )
    val namespaceClass = namespaceClassMap.get(namespace) match {
      case Some(classId) => classId
      case None => "al"
    }
  }

  case class DDay(dateText: String) extends NamuMark {
    override def mkString = {
      Try(LocalDate.parse(dateText)) match {
        case Success(date) =>
          val day = ChronoUnit.DAYS.between(date, currTime)
          if (day >= 0) s"+$day"
          else s"$day"
        case Failure(_) => s"[dday($dateText)]"
      }
    }
  }

  case object DateMacro extends NamuMark {
    // calculate it based by the parsed time of json file
    override def mkString = currTime.format(dateMacroFormatter)
  }

  case object FootNoteList extends NamuMark {
    override def mkString = "[각주]"
  }

  case object TableOfContents extends NamuMark {
    override def mkString = "[목차]"
  }

  case class Include(rawHref: String, args: Map[String, String]) extends NamuMark {
    override def mkString = {
      val argString = escapeHTML(args.mkString(", "))
      val href = escapeURL(rawHref)
      s"[include(<a href=${toQ(s"entry://$href")}>$rawHref</a>), args:$argString)]"
    }
  }

  case class YoutubeLink(id: String, args: Map[String, String]) extends NamuMark {
    // Fallback to Link (For MDict)
    override def mkString = s"<a href=${toQ(s"entry://https://www.youtube.com/watch?v=$id")}>[유튜브 링크]</a>"
  }


  ////// ------ Double Bracket Links ------ //////


  // [[파일:$href|$htmlOption]]
  case class FileLink(href: String, htmlOption: Map[String, String]) extends NamuMark {
    // Fallback to Link (for Mdict)
    override def mkString = s"<a href=${toQ(s"entry://$href")}>[파일:$href]</a>"
  }
  // [[분류:$docType]]
  case class DocType(docType: String) extends NamuMark {
    override def mkString = s"<div ${c(docTypeClass)}>" +
      s"분류: <a $anchorClass href=${toQ(s"entry://분류:${escapeURL(docText)}")}>$docText</a></div>"

    val isBlur = docType.endsWith("#blur")
    val docText = if (isBlur) docType.take(docType.length - 5) else docType
    private val anchorClass = if (isBlur) c(blurClass) else ""
  }

  // [[$href|$alias]] -> href will be changed to NormalHref after the postprocessing
  // so this should not be escape punctuations like " " -> "%20"
  case class DocLink(href: NamuHref, valueSeq: Option[NamuMark])
    extends HasNamuSeq[NamuMark, Option[NamuMark]](valueSeq) with HasHref {
    override def mkString = valueSeq match {
      case Some(nm) => s"<a href=${toQ(href.escapeValue)}>${nm.mkString}</a>"
      case None => s"<a href=${toQ(href.escapeValue)}>${href.value}</a>"
    }

    def constructorSeq(nm: Option[NamuMark]) = DocLink(href, nm)
    def hrefConstructor(href: NamuHref): NamuMark = DocLink(href, valueSeq)
  }


  ////// ------ Curly Brace Blocks ------ //////


  // {{{#!syntax $language $value}}}
  case class SyntaxBlock(language: String, value: String) extends NamuMark {
    override def mkString = s"<pre><code>$value</code></pre>"
  }
  // {{{#!folding $title\n $value}}}
  case class FoldingBlock(title: String, value: NamuMark) extends NamuMark {
    override def mkString =
      s"<dl ${c(foldingClass)}>" +
        s"<dt>$title</dt>" +
        s"<dd>${value.mkString}</dd></dl>"
  }

  // {{{#!wiki style="$style" $value}}}
  case class WikiBlock(style: String, value: NamuMark) extends NamuMark {
    override def mkString = s"<div style=${toQ(style)}>${value.mkString}</div>"
  }
  // {{|$value|}}
  case class WordBox(value: NamuMark) extends NamuMark with HasNamu {
    override def mkString = s"<table ${c(wordBoxClass)}><tbody><tr><td><p>${value.mkString}</p></td></tr></tbody></table>"
    def constructor(nm: NamuMark) = WordBox(nm)
  }
  // {{{+$size $value}}}
  case class SizeBlock(value: NamuMark, size: Int) extends HasNamu {
    override def mkString = s"<font size=${toQ(s"+$size")}>${value.mkString}</font>"
    def constructor(nm: NamuMark) = SizeBlock(nm, size)
  }
  // {{{#color $value}}}
  case class ColorBlock(value: NamuMark, color: String) extends HasNamu {
    override def mkString = s"<font color=${toQ(color)}>${value.mkString}</font>"
    def constructor(nm: NamuMark) = ColorBlock(nm, color)
  }


  ////// ------ One-Liners ------- //////

  // <math>abcd</math> -> MathBlock("abcd")
  case class MathBlock(value: String) extends NamuMark

  // ##Comment -> Comment("Comment")
  case class Comment(value: String) extends NamuMark

  // === Heading === -> RawHeadings(RawString("Heading"), 3)
  case class RawHeadings(value: NamuMark, size: Int) extends HasNamu {
    override def mkString = s"<h$size>${value.mkString}</h$size><hr>"
    def constructor(nm: NamuMark) = RawHeadings(nm, size)
  }

  // Post Process Only AST Node
  case class Headings(value: NamuMark, no: Seq[Int]) extends HasNamu {
    override def mkString = {
      val hsize = if (no.lengthCompare(5) <= 0) no.length + 1 else 6
      val hno = no.mkString(".")
      s"<h$hsize><a name=${toQ(s"s-$hno")}><a href=${toQ(s"#$tocReference")}>$hno. </a></a>${value.mkString}</h$hsize>"
    }
    def constructor(nm: NamuMark) = Headings(nm, no)
  }


  ////// ------ Span Marks ------ //////


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

  case class Redirect(href: NamuHref) extends NamuMark with HasHref {
    def hrefConstructor(href: NamuHref): NamuMark = Redirect(href)
    override def mkString = s"<a href=${toQ(href.escapeValue)}>리다이렉트:${href.value}</a>"
  }


  ////// ------ Basic Blocks ------- //////


  // HTML Unescaped String {{{#!html ... }}}
  case class HTMLString(value: String) extends NamuMark {
    override def mkString = value
  }
  // HTML Escaped Normal String
  case class RawString(value: String) extends NamuMark {
    override def mkString = value
  }
  // Markup Ignored String {{{ ... }}}
  case class InlineString(value: String, isMultiLine: Boolean) extends NamuMark {
    override def mkString =
      if (isMultiLine) s"<pre><code>$value</code></pre>" else s"<code>$value</code>"
  }
  // [br] -> BR
  case object BR extends NamuMark {
    override def mkString = "<br>"
  }
  // ---- ~ ---------- (4 to 10 times)
  case object HR extends NamuMark {
    override def mkString = "<hr>"
  }


  ////// ------ Private Utility Functions ------ //////


  private val currTime = LocalDateTime.now()
  private val dateMacroFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd kk:mm:ss")

  // 최대한 Paragraph라는 Seq[NamuMark] Wrapper를 줄이기
  private[biryo] def pbResolver(pb: ParagraphBuilder): NamuMark = {
    if (pb.markList.isEmpty) {
      RawString(pb.sb.toString)
    } else if (pb.sb.isEmpty) {
      if (pb.markList.lengthCompare(1) == 0)
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

