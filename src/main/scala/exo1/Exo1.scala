package exo1

object Exo1 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo1/input1.txt")
    try {
      val lines = src.getLines()
      // (current max, on-going sum)
      val result = lines.foldLeft((0L, 0L)) {
        case ((currentMax, onGoingSum), line) =>
          if (line == "")
            math.max(currentMax, onGoingSum) -> 0L
          else
            currentMax -> (onGoingSum + line.toLong)
      }
      println(result)
    } finally {
      src.close()
    }
  }
}
