package exo1

object Exo1bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo1/input1.txt")
    try {
      val lines = src.getLines()
      // (top1, top2, top3, on-going sum)
      val (top1, top2, top3, _) = lines.foldLeft((0L, 0L, 0L, 0L)) {
        case ((top1, top2, top3, onGoingSum), line) =>
          if (line == "") {
            val ordered = Seq(top1, top2, top3, onGoingSum).sorted.reverse
            (
              ordered.head,
              ordered(1),
              ordered(2),
              0L
            )
          } else
            (top1, top2, top3, onGoingSum + line.toLong)
      }
      println(top1 + top2 + top3)
    } finally {
      src.close()
    }
  }
}
