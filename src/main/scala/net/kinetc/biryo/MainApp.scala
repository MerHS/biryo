package net.kinetc.biryo

import java.io.File

import akka.actor.ActorSystem
import jawn.{AsyncParser, ParseException, ast}

import scala.annotation.tailrec
import scala.io.Source

object MainApp extends App {
  import MDictMaker._

  val helpText =
    """
      |usage: java -jar biryo.jar [-inline|-raw] <filename>
    """.stripMargin

  // ---- parsing arguments ----

  var filename = ""
  var useInlineCSS = false
  var printRaw = false

  if (args.length == 0) {
    throw new IllegalArgumentException(helpText)
  } else if (args.length == 1) {
    filename = args(0)
  } else {
    args(0) match {
      case "-inline" =>
        useInlineCSS = true
      case "-raw" =>
        printRaw = true
      case _ =>
        throw new IllegalArgumentException(helpText)
    }

    filename = args(1)
  }

  val namuFile = new java.io.File(filename)
  if (!namuFile.exists)
    throw new IllegalArgumentException(filename + "does not exist.")
  
  // ---- read files ----

  val frameSourceFolder = "./mdict-data/frame"

  val exportFile = if (useInlineCSS) "namu_inline.txt" else "namu.txt"

  val p = ast.JParser.async(mode = AsyncParser.UnwrapArray)

  val cssSource = Source.fromFile("./mdict-data/biryo.min.css", "UTF-8")
  HTMLRenderer.inlineStyle = cssSource.getLines.map(_.trim).mkString("\n")
  HTMLRenderer.useInlineCSS = useInlineCSS

  val fsfFile = new File(frameSourceFolder)
  if (!fsfFile.exists)
    fsfFile.mkdir()

  val namuSource = Source.fromFile(filename, "UTF-8")
  val chunks: Iterator[String] = namuSource.grouped(100000).map(_.mkString)

  // ---- making Actors ----

  val actorSystem = ActorSystem("namuParser")
  val printer = actorSystem.actorOf(PrinterActor.props(exportFile), "printerActor")
  val framePrinter = actorSystem.actorOf(FramePrinterActor.props(frameSourceFolder), "framePrinterActor")
  val mdictMakers = Array(
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker1"),
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker2"),
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker3")
  )
  var rrIndex = 0 // round robin / we can make it better
  var docCount = 0


  def makeMDict(js: ast.JValue): Unit = {
    var isFrame = false
    val prefix = js.get("namespace").getString match {
      case Some("0") => ""
      case Some("1") => isFrame = true; "틀:"
      case Some("2") => "분류:"
      case Some("6") => "나무위키:"
      case _ => return
    }
    (js.get("title").getString, js.get("text").getString) match {
      case (Some(title), Some(text)) =>
        if (!printRaw && !useInlineCSS && isFrame) {
          mdictMakers(rrIndex) ! FrameDoc(title, text)
          rrIndex = (rrIndex + 1) % 3
        }
      
        mdictMakers(rrIndex) ! MDictDoc(prefix + title, text, printRaw)
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

