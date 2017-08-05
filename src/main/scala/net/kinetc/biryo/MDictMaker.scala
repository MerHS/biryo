package net.kinetc.biryo

import akka.actor.{Actor, ActorRef, Props}
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

/**
  * Created by KINETC on 2017-07-27.
  */
object MDictMaker {
  def props(printActor: ActorRef) = Props(new MDictMaker(printActor))
  final case class MDictDoc(title: String, text: String)
  case object ParseEnd
}

class MDictMaker(printActor: ActorRef) extends Actor {
  import PrinterActor._
  import MDictMaker._

  def makeMDictHtml(title: String, text: String): Unit = {
    val parser = new WikiParser(text)
    val renderer = new HTMLRenderer
    val postProcessor = new ASTPostProcessor(title)

    parser.NamuMarkRule.run() match {
      case Success(result) =>
        val postResult = postProcessor.postProcessAST(result)
        val compiledText = title + "\n" + renderer.generateHTML(title, postResult) + "\n</>"
        printActor ! PrintText(compiledText)
      case Failure(e: ParseError) => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e)             => e.printStackTrace()
    }
  }


  def receive = {
    case MDictDoc(title, text) => makeMDictHtml(title, text)
    case ParseEnd => printActor ! Close
  }
}
