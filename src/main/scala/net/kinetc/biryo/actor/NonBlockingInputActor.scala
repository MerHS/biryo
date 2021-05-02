package net.kinetc.biryo.actor

import akka.actor.{Actor, ActorRef, Props}
import net.kinetc.biryo.actor.JsonParserActor.FinishRead
import net.kinetc.biryo.parser.JsonParser.ParseOptions
import net.kinetc.biryo.parser.{FileReadStream, JsonParser}

object NonBlockingInputActor {
  def props(options: ParseOptions, jsonParserActor: ActorRef) =
    Props(new NonBlockingInputActor(options, jsonParserActor))
}

class NonBlockingInputActor(
    val options: ParseOptions,
    val jsonParserActor: ActorRef
) extends Actor
    with FileReadStream {
  import JsonParser._

  override def sink(chunk: String): Unit = {
    jsonParserActor ! chunk
  }

  override def finish(): Unit = {
    jsonParserActor ! FinishRead
  }

  def receive = { case DoParse(fileName) =>
    readFile(fileName)
  }
}
