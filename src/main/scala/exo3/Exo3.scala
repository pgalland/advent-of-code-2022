package exo3

object Exo3 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo3/input.txt")
    try {
      val lines = src.getLines()
      val result = lines.map { line =>
        val (left, right)         = line.splitAt(line.length / 2)
        val typeInBothCompartment = left.toCharArray.intersect(right.toCharArray).head
        priority(typeInBothCompartment).toLong
      }.sum
      println(result)
    } finally {
      src.close()
    }
  }

  private def priority(c: Char): Int = {
    if (c.isLower)
      c.toInt - 96
    else
      c.toInt - 64 + 26
  }
}
