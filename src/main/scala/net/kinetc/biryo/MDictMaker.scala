package net.kinetc.biryo

import akka.actor.{Actor, ActorRef, Props}
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.util.{Failure, Success}

/**
  * Created by KINETC on 2017-07-27.
  */
object MDictMaker {
  def props(printActor: ActorRef, framePrinterActor: ActorRef)
  = Props(new MDictMaker(printActor, framePrinterActor))
  final case class MDictDoc(title: String, text: String)

  final case class FrameDoc(title: String, text: String)
  case object ParseEnd
}

class MDictMaker(printActor: ActorRef, framePrinterActor: ActorRef) extends Actor {

  import FramePrinterActor._
  import MDictMaker._
  import PrinterActor._

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

  def makeFrameJS(title: String, text: String): Unit = {
    val parser = new WikiParser(text)
    val renderer = new FrameRenderer
    val postProcessor = new ASTPostProcessor(title)

    parser.NamuMarkRule.run() match {
      case Success(result) =>
        val postResult = postProcessor.postProcessAST(result)
        val compiledText = renderer.generateHTML(title, postResult)
        framePrinterActor ! MakeJSFile(title, compiledText)
      case Failure(e: ParseError) => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e) => e.printStackTrace()
    }
  }

  def receive = {
    case MDictDoc(title, text) => makeMDictHtml(title, text)
    case FrameDoc(title, text) => makeFrameJS(title, text)
    case ParseEnd => printActor ! Close
  }
}
