package net.kinetc.biryo

import java.io.PrintWriter

import akka.actor.{Actor, ActorRef, Props}

object ExitActor {
  def props(): Props = Props(new ExitActor)

  case object Exit
}

class ExitActor extends Actor {

  import ExitActor._

  var exitCount = 0
  def receive = {

    case Exit =>
      exitCount += 1
      if (exitCount == 6) {
        println("Shutdown Program!")
        context.system.terminate()
      }
  }
}