package net.kinetc.biryo.parser

import akka.actor.ActorRef
import org.typelevel.jawn.{AsyncParser, ast}
import net.kinetc.biryo.actor.MDictMaker.{FrameDoc, MDictDoc}

object JsonParser {
  final case class ParseOptions(
      printRaw: Boolean,
      useInlineCSS: Boolean,
      blocking: Boolean
  )
  final case class DoParse(fileName: String)
}

trait JsonParser {
  import JsonParser._

  val options: ParseOptions
  val mdictMakerRouter: ActorRef

  var docCount = 0
  val p = ast.JParser.async(mode = AsyncParser.UnwrapArray)

  protected def sendMDictData(
      title: String,
      text: String,
      prefix: String,
      isFrame: Boolean
  ): Unit = {
    if (!options.printRaw && !options.useInlineCSS && isFrame) {
      mdictMakerRouter ! FrameDoc(title, text)
    }

    mdictMakerRouter ! MDictDoc(prefix + title, text, options.printRaw)
    docCount += 1
    if (docCount % 10000 == 0) {
      println(s"parse count $docCount")
    }
  }

  protected def makeMDict(js: ast.JValue): Unit = {
    var isFrame = false

    val namespace = js.get("namespace")
    val prefix = namespace.getInt match {
      case Some(0) => ""
      case Some(1) => isFrame = true; "틀:"
      case Some(2) => "분류:"
      case Some(6) => "나무위키:"
      case _ => {
        // fall back to string
        namespace.getString match {
          case Some("0") => ""
          case Some("1") => isFrame = true; "틀:"
          case Some("2") => "분류:"
          case Some("6") => "나무위키:"
          case _ => {
            println(
              s"json namespace error: ${js.get("title").getString} / ${js.get("namespace")}"
            )
            return
          }
        }
      }
    }
    (js.get("title").getString, js.get("text").getString) match {
      case (Some(title), Some(text)) =>
        sendMDictData(title, text, prefix, isFrame)
      case _ => {
        println(s"json parse error: ${js.get("title")} / ${js.get("text")}")
      }
    }
  }

  protected final def absorb(chunk: String): Unit = {
    p.absorb(chunk) match {
      case Right(js) =>
        js.foreach(makeMDict)
      case Left(e) =>
        e.printStackTrace()
    }
  }

  protected final def finishParse(): Unit = {
    p.finish() match {
      case Right(js) =>
        js.foreach(makeMDict)
      case Left(e) =>
        e.printStackTrace()
    }
  }
}
