package net.kinetc.biryo

import java.io.PrintWriter

import akka.actor.{Actor, ActorRef, Props}

/**
  * Created by KINETC on 2017-08-15.
  */
object FramePrinterActor {
  def props(path: String, exitActor: ActorRef): Props = Props(new FramePrinterActor(path, exitActor))

  final case class MakeJSFile(title: String, text: String)
  case object CloseFPA
}


class FramePrinterActor(path: String, exitActor: ActorRef) extends Actor {

  import FramePrinterActor._

  def receive = {
    case MakeJSFile(title, text) =>
      var newTitle = title.replaceAll(" ", "_")
      newTitle = "틀-" + HTMLRenderer.escapePathStr(newTitle)
      val pathFile: PrintWriter = new PrintWriter(path + "/" + newTitle + ".js", "UTF-8")
      pathFile.println("var x='" + HTMLRenderer.escapeJSStr(text) + "\';")
      pathFile.close()
    case CloseFPA =>
      exitActor ! ExitActor.Exit
  }
}
