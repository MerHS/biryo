package net.kinetc.biryo.actor

import akka.actor.{Actor, Props}

object ExitActor {
  var shutdownCount = {
    val coreSize = Runtime.getRuntime.availableProcessors
    if (coreSize == 1) 1 else coreSize - 1
  } * 2

  def props(): Props = Props(new ExitActor)

  case object Exit
}

class ExitActor extends Actor {
  import ExitActor._


  var exitCount = 0
  def receive = {

    case Exit =>
      exitCount += 1
      if (exitCount == shutdownCount) {
        println("Shutdown Program!")
        context.system.terminate()
      }
  }
}