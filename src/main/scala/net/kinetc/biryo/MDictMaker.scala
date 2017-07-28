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
  val renderer: HTMLRenderer = new HTMLRenderer

  def makeMdictHtml(title: String, text: String): Unit = {
    val parser = new WikiParser(text)
    val postProcessor = new ASTPostProcessor(title)

    parser.NamuMarkRule.run() match {
      case Success(result)        => {
        val postResult = postProcessor.postProcessAST(result)
        pathFile.println(title)
        pathFile.println(renderer generateHTML (title, postResult))
        pathFile.println("</>")
      }
      case Failure(e: ParseError) => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e)             => e.printStackTrace()
    }
  }
  def finalizeThis() = pathFile.close()
}
