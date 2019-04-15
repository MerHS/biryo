package net.kinetc.biryo.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import net.kinetc.biryo.parser.WikiParser
import net.kinetc.biryo.renderer.{ASTPostProcessor, FrameRenderer, HTMLRenderer, KatexRenderer}
import org.parboiled2.{ErrorFormatter, ParseError}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * Created by KINETC on 2017-07-27.
  */
object MDictMaker {
  def props(printActor: ActorRef, framePrinterActor: ActorRef) = 
    Props(new MDictMaker(printActor, framePrinterActor))

  final case class MDictDoc(title: String, text: String, printRaw: Boolean=false)
  final case class FrameDoc(title: String, text: String)
  case object ParseEnd
}

class MDictMaker(printActor: ActorRef, framePrinterActor: ActorRef) extends Actor {

  import FramePrinterActor._
  import MDictMaker._
  import PrinterActor._

  implicit val ec: ExecutionContext = context.system.dispatchers.lookup("biryo-blocking-dispatcher")
  val katex = new KatexRenderer

  implicit val askTimeout = Timeout(1 minutes)
  val compileTimeout = Timeout(10 seconds)

  var sendCount = 0

  def makeMDictHtml(title: String, text: String): Unit = {
    val parser = new WikiParser(text)
    val renderer = new HTMLRenderer(katex)
    val postProcessor = new ASTPostProcessor(title)

    val futureText: Future[Option[String]] = Future {
      parser.NamuMarkRule.run() match {
        case Success(result) =>
          val postResult = postProcessor.postProcessAST(result)
          val compiledText = title + "\n" + renderer.generateHTML(title, postResult) + "\n</>"

          Some(compiledText)
        case Failure(e: ParseError) =>
          println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
          None
        case Failure(e) =>
          e.printStackTrace()
          None
      }
    }

    Try(Await.result(futureText, compileTimeout.duration)) match {
      case Success(Some(compiledText)) =>
        printActor ! PrintText(compiledText)
      case _ =>
        printActor ! GetError(title)
    }
  }

  sendCount += 1
  if (sendCount % 1000 == 0) {
    println(s"Actor ${self.path.name}: $sendCount")
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

    val futureText: Future[Option[String]] = Future {
      parser.NamuMarkRule.run() match {
        case Success(result) =>
          val postResult = postProcessor.postProcessAST(result)
          val compiledText = renderer.generateHTML(title, postResult)

          Some(compiledText)
        case Failure(e: ParseError) =>
          println(parser.formatError(e, new ErrorFormatter(showTraces = true)))
          None
        case Failure(e) =>
          e.printStackTrace()
          None
      }
    }

    Try(Await.result(futureText, compileTimeout.duration)) match {
      case Success(Some(compiledText)) =>
        framePrinterActor ! MakeJSFile(title, compiledText)
      case _ =>
        printActor ! GetError(s"$title - frame")
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
  }
}
