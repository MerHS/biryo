package net.kinetc.biryo.renderer

class KatexRenderer {
//  private val engine: ScriptEngine with Invocable = {
//    val engineManager = new ScriptEngineManager()
//    var _engine = engineManager.getEngineByName("nashorn")
//    if (_engine == null) {
//      _engine = engineManager.getEngineByName("javascript")
//    }
//    if (_engine == null) {
//      _engine = engineManager.getEngineByName("rhino")
//    }
//
//    val filePath = new File("./mdict-data/math.js")
//    var canLoadJs = false
//
//    if (_engine != null && filePath.exists) {
//      try {
//        val mathSource = Source.fromFile("./mdict-data/math.js", "UTF-8")
//        val script = mathSource.getLines.mkString("\n")
//        mathSource.close()
//        _engine.asInstanceOf[ScriptEngine with Invocable].eval(script)
//        canLoadJs = true
//      } catch {
//        case NonFatal(_) => canLoadJs = true
//      }
//    }
//
//    if (canLoadJs) {
//      val __engine = _engine.asInstanceOf[ScriptEngine with Invocable]
//      __engine.invokeFunction("render", "x")
//      __engine
//    } else {
//      null
//    }
//  }

  def renderToString(mathText: String): String = {
//    if (!HTMLRenderer.useInlineCSS && engine != null) {
//      Try(engine.invokeFunction("render", mathText)) match {
//        case Success(parsedText) => parsedText.asInstanceOf[String]
//        case _ => s"<code>${HTMLRenderer.escapeHTML(mathText)}</code>"
//      }
//    } else {
//      s"<code>${HTMLRenderer.escapeHTML(mathText)}</code>"
//    }

    s"<code>${HTMLRenderer.escapeHTML(mathText)}</code>"
  }
}
