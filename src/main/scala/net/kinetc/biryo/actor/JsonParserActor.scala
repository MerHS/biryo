package net.kinetc.biryo.actor

import akka.actor.{Actor, ActorRef, Props}
import akka.routing.Broadcast
import net.kinetc.biryo.actor.MDictMaker.ParseEnd
import net.kinetc.biryo.parser.JsonParser
import net.kinetc.biryo.parser.JsonParser.ParseOptions

object JsonParserActor {
  def props(options: ParseOptions, mdictMakerRouter: ActorRef) = Props(new JsonParserActor(options, mdictMakerRouter))
  case object FinishRead
}

class JsonParserActor(val options: ParseOptions, val mdictMakerRouter: ActorRef)
  extends Actor with JsonParser {
  import JsonParserActor._

  override def receive = {
    case chunk: String =>
      absorb(chunk)
    case FinishRead =>
      finishParse()
      println(s"Finish! Document counts: $docCount")
      mdictMakerRouter ! Broadcast(ParseEnd)
  }
}
