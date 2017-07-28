package net.kinetc.biryo

import NamuAST._

/**
  * Created by KINETC on 2017-07-28.
  */
class ASTPostProcessor(val title: String) {
  def postProcessAST(mark: NamuMark): NamuMark = {
    mark.dfsMap(postProcessor)
  }

  // TODO: {{{ }}} -> code / {{{\n \n}}} -> pre code
  // TODO: Comment 직후 BR 삭제
  // TODO: SuperDocHref / ChildDocHref / RedirectHref 처리
  // TODO: 각주 번호, 문단 단계 처리 / List, Indent 처리 / Anchor 처리 / 다중 SpanMark 앞뒤공백 제거
  protected def postProcessor: NamuMap = {
    case withHref: HasHref => withHref.hrefMap(hrefPostProcesser)
    case m => m
  }

  protected def hrefPostProcesser(href: NamuHref): NamuHref = {
    href
  }
}
