package exo3

object Exo3bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo3/input.txt")
    try {
      val lines = src.getLines()
      val result = lines.grouped(3).map { groupLines =>
        val rucksacks = groupLines.map(_.toCharArray)
        val badgeType = rucksacks(0).intersect(rucksacks(1)).intersect(rucksacks(2)).head
        priority(badgeType).toLong
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
