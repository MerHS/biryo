package net.kinetc.biryo.parser

import java.io.{File, FileInputStream}

trait FileReadStream {
  def sink(chunk: String): Unit
  def finish(): Unit

  final def readFile(fileName: String): Unit = {
    val fis = new FileInputStream(new File(fileName))
    val buffer = Array.fill[Byte](1024 * 1024)(0)

    var reading = true
    while (reading) {
      val readCount = fis.read(buffer)

      if (readCount < 0) {
        reading = false
      } else if (readCount < buffer.length) {
        sink(new String(buffer.take(readCount)))
        reading = false
      } else {
        sink(new String(buffer))
      }
    }
    fis.close()

    finish()
  }
}
