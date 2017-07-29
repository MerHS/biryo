package net.kinetc.biryo

import java.io.PrintWriter
import java.util.Date

import akka.actor.{Actor, Props}

/**
  * Created by KINETC on 2017-07-29.
  */
object FileIOActor {
  def props(path: String): Props = Props(new FileIOActor(path))
  final case class PrintText(text: String)
  case object Close
}


class FileIOActor(path: String) extends Actor {
  import FileIOActor._

  val pathFile: PrintWriter = new PrintWriter(path)
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
        println(s"parsed: $parsedNo   " +
          s"| elapsed: ${(newTime.getTime - time.getTime)/1000}    " +
          s"| total: ${(newTime.getTime - oldTime.getTime)/1000}")
        time = newTime
      }
    case Close =>
      closeCount += 1
      if (closeCount == 3) pathFile.close()
  }
}
