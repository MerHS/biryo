package net.kinetc.biryo

import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorSystem}
import akka.routing.SmallestMailboxPool
import com.typesafe.config.ConfigFactory
import net.kinetc.biryo.actor._
import net.kinetc.biryo.parser.JsonParser.{DoParse, ParseOptions}
import net.kinetc.biryo.renderer.HTMLRenderer

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source

object MainApp extends App {

  val helpText =
    """
      |usage: java -jar biryo.jar [-inline|-raw|-thread [number]] <filename>
      |options:
      |  -inline: CSS 값을 link로 빼지 않고 각 문서에 인라이닝 시킵니다.
      |           mdd 파일을 읽지 못하는 구형 MDict, PMP에서 문서를 읽을 시 이 옵션을 적용해야 합니다.
      |  -thread (숫자): JSON 파서 스레드(1개) + MDict 데이터 생성 스레드(n-1개)의 개수를 조정합니다. (최소 2)
      |                  CPU가 4스레드 이하일시 디폴트 4, 초과시 (CPU 스레드 수 - 2) 입니다. (2는 IO 스레드용)
      |  -raw: 나무마크를 파싱하지 않고 나무위키 문법이 그대로 적힌 문서를 만듭니다.
    """.stripMargin

  // ---- Parsing Arguments ----

  var filename = ""
  var useInlineCSS = false
  var printRaw = false
  var blocking = true
  var poolSize = {
    val coreSize = Runtime.getRuntime.availableProcessors
    if (coreSize == 1)
      1
    else if (coreSize <= 4)
      3
    else
      coreSize - 3
  }

  if (args.length == 0) {
    throw new IllegalArgumentException(helpText)
  }
  if (args.contains("-inline")) {
    useInlineCSS = true
    HTMLRenderer.useInlineCSS = useInlineCSS
  }
  if (args.contains("-raw")) {
    printRaw = true
  }
  if (args.contains("-nonblock")) {
    // blocking = false
  }
  if (args.contains("-thread")) {
    val argPos = args.indexOf("-thread")

    if (
      argPos + 1 < args.length && (args(argPos + 1) forall Character.isDigit)
    ) {
      poolSize = args(argPos + 1).toInt - 1
      if (poolSize <= 0) {
        throw new IllegalArgumentException(
          s"error: -thread 값이 2 미만입니다\n$helpText"
        )
      }
    } else {
      throw new IllegalArgumentException(
        s"error: -thread 값에 오류가 있습니다\n$helpText"
      )
    }
  }

  ExitActor.shutdownCount = poolSize * 2

  filename = args.last

  val exportFile = if (useInlineCSS) "namu_inline.txt" else "namu.txt"

  // ---- Reading Files ----

  val namuFile = new File(filename)
  if (!namuFile.exists)
    throw new IllegalArgumentException(filename + "does not exist.")

  val frameSourceFolderPath = "./mdict-data"
  val fsfFile = new File(frameSourceFolderPath)

  if (!printRaw && fsfFile.exists && fsfFile.isDirectory) {
    fsfFile
      .listFiles()
      .filter(_.getName.startsWith("틀-"))
      .foreach(_.delete())

    val cssSource = Source.fromFile("./mdict-data/biryo.min.css", "UTF-8")
    HTMLRenderer.inlineStyle = cssSource.getLines().map(_.trim).mkString("\n")
    cssSource.close()
  } else {
    throw new IllegalArgumentException(
      frameSourceFolderPath + " folder does not exist."
    )
  }

  // ---- Making Actors ----

  val config = ConfigFactory.parseString("""
      |biryo-blocking-dispatcher {
      |  type = Dispatcher
      |  executor = "thread-pool-executor"
      |  thread-pool-executor {
      |    fixed-pool-size = 16
      |  }
      |  throughput = 1
      |}
    """.stripMargin)

  println(s"Parse Start with ${poolSize + 1} NamuMark Parser Threads")

  val actorSystem = ActorSystem("namuParser", ConfigFactory.load(config))
  val exitActor = actorSystem.actorOf(ExitActor.props(), "exitActor")
  val printer = actorSystem.actorOf(
    PrinterActor.props(exportFile, exitActor),
    "printerActor"
  )
  val framePrinter = actorSystem.actorOf(
    FramePrinterActor.props(frameSourceFolderPath, exitActor),
    "framePrinterActor"
  )
  val mdictMakerRouter = actorSystem.actorOf(
    MDictMaker
      .props(printer, framePrinter)
      .withRouter(SmallestMailboxPool(poolSize)),
    "mdictMaker"
  )

  val parseOptions = ParseOptions(printRaw, useInlineCSS, blocking)

  val jsonParserActor = actorSystem.actorOf(
    JsonParserActor.props(parseOptions, mdictMakerRouter),
    "jsonParserActor"
  )
  val mainActor: ActorRef = if (blocking) {
    actorSystem.actorOf(
      BlockingInputActor.props(parseOptions, mdictMakerRouter),
      "jsonActor"
    )
  } else {
    actorSystem.actorOf(
      NonBlockingInputActor.props(parseOptions, jsonParserActor),
      "jsonActor"
    )
  }

  mainActor ! DoParse(filename)
  Await.ready(actorSystem.whenTerminated, Duration(10, TimeUnit.MINUTES))
}
