package net.kinetc.biryo.actor

import java.io.PrintWriter
import java.util.Date

import akka.actor.{Actor, ActorRef, Props}

/** Created by KINETC on 2017-07-29.
  */
object PrinterActor {
  def props(path: String, exitActor: ActorRef): Props = Props(
    new PrinterActor(path, exitActor)
  )
  final case class PrintText(text: String)
  final case class GetError(title: String, text: String)
  case object Close
}

class PrinterActor(path: String, exitActor: ActorRef) extends Actor {
  import PrinterActor._

  val pathFile: PrintWriter = new PrintWriter(path, "UTF-8")
  var parsedNo = 0
  val oldTime = new Date()
  var time = new Date()
  var closeCount = 0

  var errorList = List[(String, String)]()

  def receive = {
    case PrintText(text) =>
      pathFile.println(text)
      parsedNo += 1
      if (parsedNo % 1000 == 0) {
        val newTime = new Date()
        val (nt, tt, ot) = (newTime.getTime, time.getTime, oldTime.getTime)
        println(
          s"printed: $parsedNo   " +
            s"| elapsed: ${1f * (nt - tt) / 1000}   " +
            s"| total: ${1f * (nt - ot) / 1000}   " +
            s"| average: ${1f * (nt - ot) / parsedNo}"
        )
        time = newTime
      }
    case Close =>
      exitActor ! ExitActor.Exit
    case GetError(title, text) =>
      println(
        s"#################### TIMEOUT WHILE PARSING: $title ####################"
      )
      errorList = (title, text) :: errorList
  }

  override def postStop(): Unit = {
    println("close file")
    if (errorList.nonEmpty) {
      val errorLog =
        new PrintWriter(s"error_${System.currentTimeMillis()}.log", "UTF-8")
      errorList.foreach({
        case (title, text) => {
          println("parseError: " + title)
          errorLog.println(s"Parse Error / Timeout: $title\n$text")
        }
      })
      errorLog.close()
    }
    pathFile.close()
  }
}
