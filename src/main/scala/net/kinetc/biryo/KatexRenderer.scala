package net.kinetc.biryo

import java.io.{File, FileReader}
import javax.script._

import scala.util.{Success, Try}
import scala.util.control.NonFatal

class KatexRenderer {
  private val engine = {
    val engineManager = new ScriptEngineManager()
    var _engine = engineManager.getEngineByName("nashorn")
    if (_engine == null) {
      _engine = engineManager.getEngineByName("javascript")
    }
    if (_engine == null) {
      _engine = engineManager.getEngineByName("rhino")
    }

    val filePath = new File("./mdict-data/math.js")
    var canLoadJs = false

    if (_engine != null && filePath.exists) {
      try {
        _engine.asInstanceOf[ScriptEngine with Invocable].eval(new FileReader("./mdict-data/math.js"))
        canLoadJs = true
      } catch {
        case NonFatal(_) => canLoadJs = true
      }
    }

    if (canLoadJs) {
      _engine.asInstanceOf[ScriptEngine with Invocable]
    } else {
      null
    }
  }

  def renderToString(mathText: String): String = {
    if (!HTMLRenderer.useInlineCSS && engine != null) {
      Try(engine.invokeFunction("render", mathText)) match {
        case Success(parsedText) => parsedText.asInstanceOf[String]
        case _ => s"<code>${HTMLRenderer.escapeHTML(mathText)}</code>"
      }
    } else {
      s"<code>${HTMLRenderer.escapeHTML(mathText)}</code>"
    }
  }
}
