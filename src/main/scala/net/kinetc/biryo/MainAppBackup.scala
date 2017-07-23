package net.kinetc.biryo

//class MyActor extends Actor {
//  val log = Logging(context.system, this)
//
//  def receive = {
//    case "test" => log.info("received test")
//    case _      => log.info("received unknown message")
//  }
//}
//
//object MainAppBackup {
//  val system = ActorSystem("Hello")
//  val helloActor = system.actorOf(Props[MyActor], name="newActor")
//  helloActor ! "test"
//
//  Thread.sleep(1000)
//
//  helloActor ! "what"
//
//  system.terminate()
//  // value class?? https://blog.outsider.ne.kr/895
//
//}
