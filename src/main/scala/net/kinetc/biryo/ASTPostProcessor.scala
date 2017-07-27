package net.kinetc.biryo

import NamuAST._
/**
  * Created by KINETC on 2017-07-28.
  */
object ASTPostProcessor {
  // TODO: {{{ }}} -> code / {{{\n \n}}} -> pre code
  // TODO: Comment 직후 BR 삭제
  // TODO: Escape HTML from RS
  // TODO: SuperDocHref / ChildDocHref 처리
  // TODO: 각주 번호, 문단 단계 처리 / List, Indent 처리 / Anchor 처리 / 다중 SpanMark 앞뒤공백 제거
  def postProcessAST(mark: NamuMark): NamuMark = {
    mark match {
      case Paragraph(value) => Paragraph(value.map(postProcessAST))
      case RawString(s) => RawString(escapeHTML(s))
      case InlineString(s) => InlineString(escapeHTML(s))
      case m: HasNamu => m.map(postProcessAST)
      case m => m
    }
  }

  def generateHTML(mark:NamuMark): String = postProcessAST(mark).htmlString

  def escapeHTML(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
  }
}
