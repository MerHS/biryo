package net.kinetc.biryo

import scala.collection.immutable.Seq

object NamuAST {
  sealed trait NamuMark
  case class Paragraph(value: Seq[NamuMark]) extends NamuMark
  case class ParagraphBuilder(markList: Seq[NamuMark], sb: StringBuilder) extends NamuMark

  case class RawListObj(value: NamuMark, listType: ListType, indentSize: Int) extends NamuMark

  case class FileLink(href: String, htmlOption: Option[String]) extends NamuMark
  case class DocLink(href: NamuHref, alias: Option[NamuMark]) extends NamuMark

  case class StringBox(value: NamuMark) extends NamuMark
  case class SizeBlock(value: NamuMark, size: Int) extends NamuMark
  case class ColorBlock(value: NamuMark, color: String) extends NamuMark

  case class RawHeadings(value:NamuMark, size: Int) extends NamuMark

  sealed trait SpanMark
  case class Strike(value: NamuMark) extends NamuMark with SpanMark
  case class Sup(value: NamuMark) extends NamuMark with SpanMark
  case class Sub(value: NamuMark) extends NamuMark with SpanMark
  case class Underline(value: NamuMark) extends NamuMark with SpanMark
  case class Bold(value: NamuMark) extends NamuMark with SpanMark
  case class Italic(value: NamuMark) extends NamuMark with SpanMark

  case class Redirect(href: NamuHref) extends NamuMark
  case class Comment(value: String) extends NamuMark
  // HTML Unescaped String {{{#!html ... }}}
  case class HTMLString(value: String) extends NamuMark
  // HTML Escaped Normal String
  case class RawString(value: String) extends NamuMark
  // Markup Ignored String {{{ ... }}}
  case class InlineString(value: String) extends NamuMark
  case object BR extends NamuMark
  case object HR extends NamuMark


  sealed trait NamuHref
  // [[value]] => NormalHref("value")
  case class NormalHref(value: String) extends NamuHref
  // [[value#s-0.1.2]] => ParaHref("value", Seq[Int](0, 1, 2))
  case class ParaHref(value: String, paraNo: Seq[Int]) extends NamuHref
  // [[value#anchor]] => AnchorHref("value", "anchor")
  case class AnchorHref(value: String, anchor: String) extends NamuHref
  // [[http://example.com]] => ExternalHref("http://example.com")
  case class ExternalHref(value: String) extends NamuHref
  // [[#1.4.1]] => SelfParaHref(Seq[Int](1,4,1))
  case class SelfParaHref(paraNo: Seq[Int]) extends NamuHref
  // [[#anchor]] => SelfAnchorHref("anchor")
  case class SelfAnchorHref(anchor: String) extends NamuHref
  // [[../]] => SuperDocHref
  case object SuperDocHref extends NamuHref
  // [[/child]] => ChildDocHref("child")
  case class ChildDocHref(value: NamuHref) extends NamuHref

  sealed trait ListType
  case object Type_star extends ListType
  case object Type_1 extends ListType
  case object Type_i extends ListType
  case object Type_I extends ListType
  case object Type_A extends ListType
  case object Type_a extends ListType


  // Post Process Only AST Node
  case class Headings(value: NamuMark, no: Seq[Int]) extends NamuMark


  // TODO: 문단 단계 처리 / List, Indent 처리 / Anchor 처리 / 다중 SpanMark 앞뒤공백 제거
  def postProcessAST(mark: NamuMark): NamuMark = ???

  def generateHTML(mark:NamuMark): String = ???


  // TODO : Escape HTML from Strings
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
      ParagraphBuilder(pb.markList :+ lineObj, pb.sb)
    } else {
      ParagraphBuilder(
        pb.markList :+ RawString(pb.sb.toString) :+ lineObj,
        new StringBuilder
      )
    }
  }
}

/*
  나무위키 문법은 기본적으로 한 줄 단위로 처리된다. 
  한 줄의 문법을 parsing 가능한 최상위 Line으로 설정
*/
