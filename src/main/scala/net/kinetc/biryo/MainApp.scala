package net.kinetc.biryo

object MainApp extends App {
//   if (args.length != 1)
//     throw new IllegalArgumentException("usage: ./namuhtml-scala <filename>")
//   val fileName = args(0)
//   val namuFile = new java.io.File(filename)
//   if (namuFile.exists == false)
//     throw new IllegalArgumentException(fileName + "does not exist.")
//
//  val namuFile = "/root/namus/src/test/namutest.json"
//
//  val p = ast.JParser.async(mode = AsyncParser.UnwrapArray)
//
//  def chunks: Stream[String] =
//    Source.fromFile(namuFile).sliding(100, 100).toStream.map(_.mkString)
//
//  def sink(j: ast.JValue) =
//    j.getString match {
//      case Some(v) => println(v)
//      case _ => println("no")
//    }
//
//  def loop(st: Stream[String]): Either[ParseException, Unit] =
//    st match {
//      case s #:: tail =>
//        p.absorb(s) match {
//          case Right(js) =>
//            js.foreach(sink)
//            loop(tail)
//          case Left(e) =>
//            e.printStackTrace()
//            Left(e)
//        }
//      case _ =>
//        p.finish().right.map(_.foreach(sink))
//    }
//
//  println("start")
//
//  loop(chunks)
}

