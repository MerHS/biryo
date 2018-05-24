package net.kinetc.biryo.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.routing.Broadcast
import net.kinetc.biryo.actor.MDictMaker._
import net.kinetc.biryo.parser.JsonParser.ParseOptions
import net.kinetc.biryo.parser.{FileReadStream, JsonParser}

object BlockingInputActor {
  def props(options: ParseOptions, mdictMakerRouter: ActorRef) = Props(new BlockingInputActor(options, mdictMakerRouter))
}

class BlockingInputActor(val options: ParseOptions, val mdictMakerRouter: ActorRef)
  extends Actor with JsonParser with FileReadStream {
  import JsonParser._

  override def sink(chunk: String): Unit = {
    absorb(chunk)
  }

  override def finish(): Unit = {
    finishParse()
    println(s"Finish! Document counts: $docCount")
    mdictMakerRouter ! Broadcast(ParseEnd)
  }

  def receive = {
    case DoParse(fileName) => readFile(fileName)
  }
}
