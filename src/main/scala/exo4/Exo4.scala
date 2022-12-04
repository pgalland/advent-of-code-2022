package exo4

object Exo4 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo4/input.txt")
    try {
      val lines = src.getLines()
      val result = lines.count { line =>
        val Seq(left_lo, left_hi, right_lo, right_hi) =
          line.split(",").flatMap(_.split("-")).map(_.toLong).toSeq
        (left_lo >= right_lo && left_hi <= right_hi) || (right_lo >= left_lo && right_hi <= left_hi)
      }
      println(result)
    } finally {
      src.close()
    }
  }

}
