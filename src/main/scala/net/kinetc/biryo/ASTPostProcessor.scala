package net.kinetc.biryo

import NamuAST._

// TODO: 저주받은 루드비히 / 아이린(레드벨벳) -> 하얀색

/**
  * Created by KINETC on 2017-07-28.
  */
class ASTPostProcessor(val title: String) {
  private var hMin = 6
  private var currHeading: IndexedSeq[Int] = IndexedSeq(0)
  private var fnNo = 0

  def postProcessAST(mark: NamuMark): NamuMark = {
    mark.nfs(findIndent)

    mark.nfsMap(footNoteProcessor).cfsMap(postProcessor)
  }

  // TODO: Comment 직후 BR 삭제
  // TODO: List, Indent 처리 / 다중 SpanMark 앞뒤공백 제거
  // TODO: DocType 모으기
  // TODO: TableStyle 정렬 (TR Style, TableStyle이 각각 Table, TR에 가게)
  protected def postProcessor: NamuMap = {
    /// ----- Href Resolver -----
    case DocLink(href: ExternalHref, None) =>
      DocLink(NormalHref(href.value), Some(NamuAST.RawString("외부링크")))
    case DocLink(href: NamuHref, None) =>
      DocLink(href, Some(NamuAST.RawString(href.value))).hrefMap(hrefProcessor)
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

  protected def footNoteProcessor: NamuMap = {
    case f @ FootNote(v, noteStr) =>
      fnNo += 1
      noteStr match {
        case Some(_) => f
        case None => FootNote(v, Some(fnNo.toString))
      }
  }

  protected def findIndent(mark: NamuMark): Unit = {
    mark match {
      case RawHeadings(_, s) => if (s < hMin) hMin = s
      case _ => ()
    }
  }

  // change all NamuHrefs to NormalHref except ExternalHref
  protected def hrefProcessor(href: NamuHref): NamuHref = {
    href match {
      case NormalHref(v) => NormalHref(s"entry://$v")
      case ParaHref(v, paraNo) => NormalHref(s"entry://$v#s-${paraNo.mkString(".")}")
      case AnchorHref(value, anchor) => NormalHref(s"entry://$value#$anchor")
      case SelfParaHref(_) | SelfAnchorHref(_) => NormalHref(s"entry://${href.value}")
      case SuperDocHref =>
        val newHref =
          if (title.contains('/')) title.split("/").init.mkString("/") else title
          NormalHref(s"entry://$newHref")
      case ChildDocHref(h) => NormalHref(s"entry://$title/${hrefProcessor(h).value}")
      case _ => href
    }
  }
}
