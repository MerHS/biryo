package net.kinetc.biryo

import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import akka.pattern.ask
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * Created by KINETC on 2017-07-27.
  */
object MDictMaker {
  def props(printActor: ActorRef, framePrinterActor: ActorRef) = 
    Props(new MDictMaker(printActor, framePrinterActor))

  final case class MDictDoc(title: String, text: String, printRaw: Boolean=false)
  final case class FrameDoc(title: String, text: String)
  case object ParseEnd
  case object WaitUntilMailBoxCleared
}

class MDictMaker(printActor: ActorRef, framePrinterActor: ActorRef) extends Actor {

  import FramePrinterActor._
  import MDictMaker._
  import PrinterActor._

  val katex = new KatexRenderer
  implicit val timeout = Timeout(3 minutes)

  var sendCount = 0

  def makeMDictHtml(title: String, text: String): Unit = {
    val parser = new WikiParser(text)
    val renderer = new HTMLRenderer(katex)
    val postProcessor = new ASTPostProcessor(title)

    parser.NamuMarkRule.run() match {
      case Success(result) =>
        val postResult = postProcessor.postProcessAST(result)
        val compiledText = title + "\n" + renderer.generateHTML(title, postResult) + "\n</>"
        printActor ! PrintText(compiledText)
        sendCount += 1
        if (sendCount % 1000 == 0) {
          val future = printActor ? WaitUntilPrint
          val result = Await.result(future, timeout.duration)
          println(s"Actor ${self.path.name}: $result")
        }
      case Failure(e: ParseError) => println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
      case Failure(e)             => e.printStackTrace()
    }
  }

  def makeRawHtml(title: String, text: String): Unit = {
    printActor ! PrintText(
      title + "\n<pre>" + HTMLRenderer.escapeHTML(text) + "</pre>\n</>"
    )
  }

  def makeFrameJS(title: String, text: String): Unit = {
    val parser = new WikiParser(text)
    val renderer = new FrameRenderer(katex)
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
    case MDictDoc(title, text, printRaw) =>
      if (printRaw)
        makeRawHtml(title, text)
      else
        makeMDictHtml(title, text)
    case FrameDoc(title, text) =>
      makeFrameJS(title, text)
    case ParseEnd =>
      printActor ! Close
      framePrinterActor ! CloseFPA
    case WaitUntilMailBoxCleared =>
      sender ! "cleared!"
  }
}
