package net.kinetc.biryo

import java.io.{File, FileOutputStream, PrintWriter}

import jawn.ast
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

/**
  * Created by KINETC on 2017-07-27.
  */
class MDictMaker(path: String) {
  val pathFile: PrintWriter = new PrintWriter(path)

  def makeMdictHtml(title: String, text: String): Unit = {
    val parser = new WikiParser(text)
    parser.NamuMarkRule.run() match {
      case Success(result)        => {
        val wikiHTML = ASTPostProcessor.generateHTML(result)
        pathFile.println(title)
        pathFile.println(wikiHTML.replaceAll("\n", "<br>"))
        pathFile.println("</>")
      }
      case Failure(e: ParseError) => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e)             => e.printStackTrace()
    }
  }
  def finalizeThis() = pathFile.close()
}
