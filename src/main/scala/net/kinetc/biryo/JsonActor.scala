package net.kinetc.biryo

import scala.concurrent.Await
import scala.concurrent.duration._
import akka.util.Timeout
import akka.pattern.ask
import akka.actor.{Actor, ActorRef, Props}
import jawn.{AsyncParser, ParseException, ast}

import scala.annotation.tailrec
import scala.io.Source

object JsonActor {
  def props(args: Arguments, mdictMakers: Array[ActorRef]) = Props(new JsonActor(args, mdictMakers))

  final case class DoParse(fileName: String)
  final case class Arguments(printRaw: Boolean, useInlineCSS: Boolean, blocking: Boolean)
}

class JsonActor(args: JsonActor.Arguments, mdictMakers: Array[ActorRef]) extends Actor {
  import JsonActor._
  import net.kinetc.biryo.MDictMaker._
  var rrIndex = 0 // round robin / we can make it better
  var docCount = 0

  implicit val timeout = Timeout(3 minutes)

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
          mdictMakers(rrIndex) ! FrameDoc(title, text)
          rrIndex = (rrIndex + 1) % 3
        }

        mdictMakers(rrIndex) ! MDictDoc(prefix + title, text, args.printRaw)
        rrIndex = (rrIndex + 1) % 3
        docCount += 1
        if (args.blocking && docCount % 10000 == 0) {
          val future = mdictMakers(0) ? WaitUntilMailBoxCleared
          val result = Await.result(future, timeout.duration)
          println(result)
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

      mdictMakers.foreach(_ ! ParseEnd)
  }
}
