package net.kinetc.biryo

import java.io.File

import akka.actor.ActorSystem
import jawn.{AsyncParser, ParseException, ast}
import net.kinetc.biryo.JsonActor.{Arguments, DoParse}

import scala.annotation.tailrec
import scala.io.Source

object MainApp extends App {
  import MDictMaker._

  val helpText =
    """
      |usage: java -jar biryo.jar [-inline|-raw] <filename>
    """.stripMargin


  // ---- Parsing Arguments ----


  var filename = ""
  var useInlineCSS = false
  var printRaw = false
  var blocking = false

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
      case "-block" =>
        blocking = true
      case _ =>
        throw new IllegalArgumentException(helpText)
    }

    filename = args(1)
  }

  val exportFile = if (useInlineCSS) "namu_inline.txt" else "namu.txt"


  // ---- Reading Files ----


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
  cssSource.close()


  // ---- Making Actors ----


  val actorSystem = ActorSystem("namuParser")
  val exitActor = actorSystem.actorOf(ExitActor.props(), "exitActor")
  val printer = actorSystem.actorOf(PrinterActor.props(exportFile, exitActor), "printerActor")
  val framePrinter = actorSystem.actorOf(FramePrinterActor.props(frameSourceFolderPath, exitActor), "framePrinterActor")
  val mdictMakers = Array(
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker1"),
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker2"),
    actorSystem.actorOf(MDictMaker.props(printer, framePrinter), "mdictMaker3")
  )
  val jsonActor = actorSystem.actorOf(JsonActor.props(Arguments(printRaw, useInlineCSS, blocking), mdictMakers), "jsonActor")

  jsonActor ! DoParse(filename)
}

