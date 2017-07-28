package net.kinetc.biryo

import net.kinetc.biryo.NamuAST._

/**
  * Created by KINETC on 2017-07-28.
  */
class HTMLRenderer {
  // TODO : 제목 / 각주 / 목차
  def generateHTML(title: String, mark: NamuMark): String = {
    mark.dfsMap(escapeMapper).mkString.replace("\n", "<br>")
  }

  def escapeMapper: NamuMap = {
      case RawString(s) => RawString(escapeHTML(s))
      case InlineString(s) => InlineString(escapeHTML(s))
    }


  def escapeHTML(s: String): String = {
    s.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;")
  }
}
