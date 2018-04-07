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
        HTMLRenderer.useInlineCSS = useInlineCSS
      case "-raw" =>
        printRaw = true
      case _ =>
        throw new IllegalArgumentException(helpText)
    }

    filename = args(1)
  }

  val exportFile = if (useInlineCSS) "namu_inline.txt" else "namu.txt"


  // ---- read files ----


  val namuFile = new File(filename)
  if (!namuFile.exists)
    throw new IllegalArgumentException(filename + "does not exist.")

  val frameSourceFolderPath = "./mdict-data"

  val fsfFile = new File(frameSourceFolderPath)
  if (fsfFile.exists && fsfFile.isDirectory) {
    fsfFile
      .listFiles()
      .filter(_.getName.startsWith("틀-"))
      .foreach(_.delete())
  } else {
    throw new IllegalArgumentException(frameSourceFolderPath + " folder does not exist.")
  }

  val cssSource = Source.fromFile("./mdict-data/biryo.min.css", "UTF-8")
  HTMLRenderer.inlineStyle = cssSource.getLines.map(_.trim).mkString("\n")

  val namuSource = Source.fromFile(filename, "UTF-8")

  // ---- making Actors ----


  val actorSystem = ActorSystem("namuParser")
  val printer = actorSystem.actorOf(PrinterActor.props(exportFile), "printerActor")
  val framePrinter = actorSystem.actorOf(FramePrinterActor.props(frameSourceFolderPath), "framePrinterActor")
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


  val p = ast.JParser.async(mode = AsyncParser.UnwrapArray)

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

  val chunks: Iterator[String] = namuSource.grouped(1024 * 64).map(_.mkString) // 64KB Chunk
  loop(chunks)
  namuSource.close()

  println(s"Finish! Document counts: $docCount")

  mdictMakers.foreach(_ ! ParseEnd)
}

