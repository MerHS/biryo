package net.kinetc.biryo

import java.io.PrintWriter
import java.util.Date

import akka.actor.{Actor, Props}

/**
  * Created by KINETC on 2017-07-29.
  */
object PrinterActor {
  def props(path: String): Props = Props(new PrinterActor(path))
  final case class PrintText(text: String)
  case object Close
}


class PrinterActor(path: String) extends Actor {
  import PrinterActor._

  val pathFile: PrintWriter = new PrintWriter(path, "UTF-8")
  var parsedNo = 0
  val oldTime = new Date()
  var time = new Date()
  var closeCount = 0

  def receive = {
    case PrintText(text) =>
      pathFile.println(text)
      parsedNo += 1
      if (parsedNo % 1000 == 0) {
        val newTime = new Date()
        val (nt, tt, ot) = (newTime.getTime, time.getTime, oldTime.getTime)
        println(s"parsed: $parsedNo   " +
          s"| elapsed: ${1f*(nt - tt)/1000}   " +
          s"| total: ${1f*(nt-ot)/1000}   " +
          s"| average: ${1f*(nt - ot)/parsedNo}")
        time = newTime
      }
    case Close =>
      println("get close")
      closeCount += 1
      if (closeCount == 3) {
        pathFile.close()
        sys.exit()
      }
  }
}
