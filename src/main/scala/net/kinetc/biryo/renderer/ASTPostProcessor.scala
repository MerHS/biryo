package net.kinetc.biryo.renderer

import net.kinetc.biryo.parser.NamuAST

/** Created by KINETC on 2017-07-28.
  */
class ASTPostProcessor(val title: String) {
  import HTMLRenderer._
  import NamuAST._

  private var hMin = 6
  private var currHeading: IndexedSeq[Int] = IndexedSeq(0)
  private var fnNo = 0

  def postProcessAST(mark: NamuMark): NamuMark = {
    mark.preTrav(findHeadings)

    mark.preMap(footNoteAndTableProcessor).postMap(postProcessor)
  }

  // TODO: List / 다중 SpanMark 앞뒤공백 제거
  protected def postProcessor: NamuMap = {
    /// ----- Href Resolver -----
    case DocLink(href: ExternalHref, None) =>
      DocLink(NormalHref(href.value), Some(NamuAST.RawString("외부링크")))

    case DocLink(href, Some(NamuAST.RawString(str))) if str.isEmpty =>
      DocLink(href, None).hrefMap(hrefProcessor)

    case withHref: HasHref =>
      withHref.hrefMap(hrefProcessor)

    /// ----- Heading Resolver -----
    case RawHeadings(v, size) =>
      val realSize = size - hMin
      if (realSize < currHeading.length) {
        currHeading = currHeading.take(realSize + 1)
        currHeading = currHeading.updated(realSize, currHeading(realSize) + 1)
      } else {
        currHeading ++= Seq.fill(realSize - currHeading.length + 1)(1)
      }
      Headings(v, currHeading)

    case HTMLString(s) if s.contains("<span") && !s.contains("</span>") =>
      HTMLString(s + "</span>")
  }

  protected def footNoteAndTableProcessor: NamuMap = {
    case f @ FootNote(v, noteStr) =>
      fnNo += 1
      noteStr match {
        case Some(_) => f
        case None    => FootNote(v, Some(fnNo.toString))
      }
    case Table(trList, styles) =>
      var tableStyle = List[TableStyle]()
      for (
        tr <- trList;
        td <- tr.valueSeq;
        style <- td.styles
      ) {
        style match {
          case f: ForTable => if (f.forTable) tableStyle ::= f
          case _           => ()
        }
      }
      Table(trList, tableStyle ++ styles)
    case TR(tdList, styles) =>
      var trStyle = List[TableStyle]()
      for (
        td <- tdList;
        style <- td.styles
      ) {
        style match {
          case f: RowBgColor => trStyle ::= f
          case _             => ()
        }
      }
      TR(tdList, trStyle ++ styles)
    case TD(nm, styles) =>
      TD(
        nm,
        styles.filterNot {
          case f: ForTable   => f.forTable
          case RowBgColor(_) => true
          case _             => false
        }
      )
  }

  protected def findHeadings(mark: NamuMark): Unit = {
    mark match {
      case RawHeadings(_, s) => if (s < hMin) hMin = s
      case _                 => ()
    }
  }

  // change all NamuHrefs to NormalHref except ExternalHref
  protected def hrefProcessor(href: NamuHref): NamuHref = {
    href match {
      case NormalHref(_) => RawHref(href.value, s"entry://${href.escapeValue}")
      case ParaHref(value, paraNo) if value == title =>
        hrefProcessor(SelfParaHref(paraNo))
      case ParaHref(value, paraNo) =>
        RawHref(value, s"entry://${href.escapeValue}#s-${paraNo.mkString(".")}")
      case AnchorHref(value, anchor) if value == title =>
        hrefProcessor(SelfAnchorHref(anchor))
      case AnchorHref(value, anchor) =>
        RawHref(value, s"entry://${href.escapeValue}#${escapeURL(anchor)}")
      case SelfParaHref(_) | SelfAnchorHref(_) =>
        RawHref(href.value, s"entry://${href.escapeValue}")
      case SuperDocHref =>
        val newHref =
          if (title.contains('/')) title.split("/").init.mkString("/")
          else title
        RawHref(newHref, s"entry://${escapeURL(newHref)}")
      case ChildDocHref(h) =>
        val childVal = hrefProcessor(h).value.replaceAll("entry://", "")
        RawHref(s"$title/$childVal", s"entry://${escapeURL(title)}/$childVal")
      case _ => href
    }
  }
}
