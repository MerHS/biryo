package net.kinetc.biryo

import jawn.{AsyncParser, ParseException, ast}

import scala.annotation.tailrec
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
//
//  var objNo = 0
//  var xNo = 0
//  def sink(js: ast.JValue) = {
//    xNo += 1
//    if (xNo % 1000 == 0)
//      println(s"parsed $xNo")
//    var noneNo = 0
//    var (s1, s2, s3, s4, s5) = (0, 0, 0, 0, 0)
//    js.get("namespace").getString match {
//      case Some("0") => {
//        if (objNo % 3000 == 0) {
//          println(s"find name1: ${js.get("title").getString}\n     text1: ${js.get("text").getString.map(_.take(30))}")
//        }
//        objNo += 1
//      }
//      case Some("1") => {
//        if (s1 % 10 == 0) {
//          println(s"find name2: ${js.get("title").getString}\n     text1: ${js.get("text").getString.map(_.take(30))}")
//        }
//        s1 += 1
//      }
//      case Some("2") => {
//        if (s2 % 10 == 0) {
//          println(s"find name3: ${js.get("title").getString}\n     text1: ${js.get("text").getString.map(_.take(30))}")
//        }
//        s2 += 1
//      }
//      case Some("3") => {
//        if (s3 % 10 == 0) {
//          println(s"find name4: ${js.get("title").getString}\n     text1: ${js.get("text").getString.map(_.take(30))}")
//        }
//        s3 += 1
//      }
//      case Some("4") => {
//        if (s4 % 10 == 0) {
//          println(s"find name5: ${js.get("title").getString}\n     text1: ${js.get("text").getString.map(_.take(30))}")
//        }
//        s4 += 1
//      }
//      case Some("5") => {
//        if (s4 % 10 == 0) {
//          println(s"find name6: ${js.get("title").getString}\n     text1: ${js.get("text").getString.map(_.take(30))}")
//        }
//        s4 += 1
//      }
//      case p @ _ => {
//        if (noneNo % 10 == 0) {
//          println(s"find ${p}: ${js.get("title").getString}\n     text1: ${js.get("text").getString.map(_.take(30))}")
//        }
//        noneNo += 1
//      }
//    }
//  }

  val mMaker = new MDictMaker("namuMDX.html")

  def makeMDict(js: ast.JValue) = {
    js.get("namespace").getString match {
      case Some("0") | Some("1") => {
        (js.get("title").getString, js.get("text").getString) match {
          case (Some(title), Some(text)) => mMaker.makeMdictHtml(title, text)
        }
      }
      case Some("2") => {
        (js.get("title").getString, js.get("text").getString) match {
          case (Some(title), Some(text)) => mMaker.makeMdictHtml("분류:" + title, text)
        }
      }
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

