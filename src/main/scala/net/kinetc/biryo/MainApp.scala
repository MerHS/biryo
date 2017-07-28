package net.kinetc.biryo

import jawn.{AsyncParser, ParseException, ast}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object MainApp extends App {
//   if (args.length != 1)
//     throw new IllegalArgumentException("usage: ./namuhtml-scala <filename>")
//   val fileName = args(0)
//   val namuFile = new java.io.File(filename)
//   if (namuFile.exists == false)
//     throw new IllegalArgumentException(fileName + "does not exist.")

  val namuFile = "../namu3.json"

  val p = ast.JParser.async(mode = AsyncParser.UnwrapArray)

  val namuSource = Source.fromFile(namuFile)
  val chunks: Iterator[String] = namuSource.grouped(20000).map(_.mkString)

  val mMaker = new MDictMaker("namuMDX.html")
  var nameSetZero = mutable.Set[String]()
  // TODO: 틀 체크!!
  def makeMDict(js: ast.JValue): Unit = {
    js.get("namespace").getString match {
      case Some("0") =>
        (js.get("title").getString, js.get("text").getString) match {
          case (Some(title), Some(text)) => {
            nameSetZero.add(title)
            mMaker.makeMdictHtml(title, text)
          }
          case _ => ()
        }
      case Some("1") =>
        (js.get("title").getString, js.get("text").getString) match {
          case (Some(title), Some(text)) => {
            val newTitle = if (nameSetZero.contains(title)) {
              println(s"틀 발견: $title")
              "틀:"+title
            } else title
            mMaker.makeMdictHtml(newTitle, text)
          }
          case _ => ()
        }
      case Some("2") =>
        (js.get("title").getString, js.get("text").getString) match {
          case (Some(title), Some(text)) => mMaker.makeMdictHtml("분류:" + title, text)
          case _ => ()
        }
      case Some("6") =>
        (js.get("title").getString, js.get("text").getString) match {
          case (Some(title), Some(text)) => mMaker.makeMdictHtml("나무위키:" + title, text)
          case _ => ()
        }
      case _ => ()
    }
  }

  @tailrec
  def loop(it: Iterator[String]): Either[ParseException, Unit] = {
    if (it.hasNext) {
      p.absorb(it.next) match {
        case Right(js) =>
          js.foreach(makeMDict)
          loop(it)
        case Left(e) =>
          e.printStackTrace()
          Left(e)
      }
    } else p.finish().right.map(_.foreach(makeMDict))
  }

  val parsed = loop(chunks)
  namuSource.close()
  mMaker.finalizeThis()
}

