package net.kinetc.biryo

import akka.actor.{Actor, ActorRef, Props}
import akka.routing.Broadcast
import jawn.{AsyncParser, ParseException, ast}

import scala.annotation.tailrec
import scala.io.Source

object JsonActor {
  def props(args: Arguments, mdictMakerRouter: ActorRef) = Props(new JsonActor(args, mdictMakerRouter))

  final case class DoParse(fileName: String)
  final case class Arguments(printRaw: Boolean, useInlineCSS: Boolean, blocking: Boolean)
}

class JsonActor(args: JsonActor.Arguments, mdictMakerRouter: ActorRef) extends Actor {
  import JsonActor._
  import net.kinetc.biryo.MDictMaker._

  var docCount = 0

  private def makeMDict(js: ast.JValue): Unit = {
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
        if (!args.printRaw && !args.useInlineCSS && isFrame) {
          mdictMakerRouter ! FrameDoc(title, text)
        }

        mdictMakerRouter ! MDictDoc(prefix + title, text, args.printRaw)
        docCount += 1
        if (docCount % 10000 == 0) {
          println(s"parse count $docCount")
        }
      case _ => ()
    }
  }


  val p = ast.JParser.async(mode = AsyncParser.UnwrapArray)

  @tailrec
  private def loop(it: Iterator[String]): Either[ParseException, Unit] = {
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

  def receive = {
    case DoParse(fileName) =>
      val namuSource = Source.fromFile(fileName, "UTF-8")
      val chunks: Iterator[String] = namuSource.grouped(1024 * 64).map(_.mkString) // 64KB Chunk
      loop(chunks)
      namuSource.close()
      println(s"Finish! Document counts: $docCount")

      mdictMakerRouter ! Broadcast(ParseEnd)
  }
}
