package net.kinetc.biryo

import java.io.File

import akka.actor.ActorSystem
import jawn.{AsyncParser, ParseException, ast}

import scala.annotation.tailrec
import scala.io.Source

object MainApp extends App {
  import MDictMaker._
//   if (args.length != 1)
//     throw new IllegalArgumentException("usage: ./namuhtml-scala <filename>")
//   val fileName = args(0)
//   val namuFile = new java.io.File(filename)
//   if (namuFile.exists == false)
//     throw new IllegalArgumentException(fileName + "does not exist.")

  val namuFile = "../namuwiki.json"
  val frameSourceFolder = "./mdict-data/frame"

  val useInlineCSS = false
  val exportFile = if (useInlineCSS) "namu_inline.txt" else "namu.txt"

  val p = ast.JParser.async(mode = AsyncParser.UnwrapArray)

  val cssSource = Source.fromFile("./mdict-data/biryo.css")
  HTMLRenderer.inlineStyle = cssSource.getLines.map(_.trim).mkString("\n")
  HTMLRenderer.useInlineCSS = useInlineCSS

  val fsfFile = new File(frameSourceFolder)
  if (!fsfFile.exists)
    fsfFile.mkdir()

  val namuSource = Source.fromFile(namuFile)
  val chunks: Iterator[String] = namuSource.grouped(100000).map(_.mkString)

  val actorSystem = ActorSystem("namuParser")
  val printer = actorSystem.actorOf(PrinterActor.props(exportFile), "printerActor")
  val framePrinter = actorSystem.actorOf(FramePrinterActor.props(frameSourceFolder), "framePrinterActor")
  val mdictMakers = Array(
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker1"),
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker2"),
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker3")
  )
  var rrIndex = 0
  var docCount = 0

  def makeMDict(js: ast.JValue): Unit = {
    var isFrame = false
    val prefix = js.get("namespace").getString match {
      case Some("0") => ""
      case Some("1") => isFrame = true; ""
      case Some("2") => "분류:"
      case Some("6") => "나무위키:"
      case _ => return
    }
    (js.get("title").getString, js.get("text").getString) match {
      case (Some(title), Some(text)) =>
        if (isFrame)
          mdictMakers(rrIndex) ! FrameDoc(title, text)
        else
          mdictMakers(rrIndex) ! MDictDoc(prefix + title, text)
        rrIndex = (rrIndex + 1) % 3
        docCount += 1
      case _ => ()
    }
  }

  @tailrec
  def loop(it: Iterator[String]): Either[ParseException, Unit] = {
    if (it.hasNext) {
      p.absorb(it.next) match {
        case Right(js) =>
          js.foreach(makeMDict)
          loop(it)
        case Left(e) =>
          e.printStackTrace()
          Left(e)
      }
    } else p.finish().right.map(_.foreach(makeMDict))
  }

  val parsed = loop(chunks)
  namuSource.close()

  println(s"Finish! Document counts: $docCount")

  mdictMakers.foreach(_ ! ParseEnd)

}

