package net.kinetc.biryo

import java.io.File

import akka.actor.ActorSystem
import akka.routing.SmallestMailboxPool
import com.typesafe.config.ConfigFactory
import net.kinetc.biryo.actor.JsonActor.{Arguments, DoParse}
import net.kinetc.biryo.actor._
import net.kinetc.biryo.renderer.HTMLRenderer

import scala.io.Source

object MainApp extends App {

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

  val config = ConfigFactory.parseString(
    """
      |biryo-blocking-dispatcher {
      |  type = Dispatcher
      |  executor = "thread-pool-executor"
      |  thread-pool-executor {
      |    fixed-pool-size = 16
      |  }
      |  throughput = 1
      |}
    """.stripMargin)

  val poolSize = {
    val coreSize = Runtime.getRuntime.availableProcessors
    if (coreSize == 1) 1 else coreSize - 1
  }

  println(s"Parse Start with ${poolSize + 1} Threads")

  val actorSystem = ActorSystem("namuParser", ConfigFactory.load(config))
  val exitActor = actorSystem.actorOf(ExitActor.props(), "exitActor")
  val printer = actorSystem.actorOf(PrinterActor.props(exportFile, exitActor), "printerActor")
  val framePrinter = actorSystem.actorOf(FramePrinterActor.props(frameSourceFolderPath, exitActor), "framePrinterActor")
  val mdictMakerRouter = actorSystem.actorOf(
    MDictMaker.props(printer, framePrinter).withRouter(SmallestMailboxPool(poolSize)),
    "mdictMaker"
  )
  val jsonActor = actorSystem.actorOf(JsonActor.props(Arguments(printRaw, useInlineCSS, blocking), mdictMakerRouter), "jsonActor")

  jsonActor ! DoParse(filename)
}

