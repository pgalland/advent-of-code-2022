package exo6

import scala.collection.mutable

object Exo6bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo6/input.txt")
    try {
      val lines = src.getLines()
      lines.foreach { line =>
        val result = (0 until line.length)
          .find(index => line.slice(index - 13, index + 1).distinct.length == 14)
          .get + 1
        println(result)
      }
    } finally {
      src.close()
    }
  }
}
