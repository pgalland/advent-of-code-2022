package exo18

import scala.collection.mutable

object Exo18 {

  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo18/input.txt")
    try {
      val lines     = src.getLines()
      val points    = lines.map(parseLine).toSeq
      val allPoints = points.toSet
      val result    = points.map(_.nbExposedSides(allPoints)).sum

      println(result)
    } finally {
      src.close()
    }
  }

  case class Point(x: Int, y: Int, z: Int) {
    def neighbours: Seq[Point] = Seq(-1, 1).flatMap(shift =>
      Seq(copy(x = x + shift), copy(y = y + shift), copy(z = z + shift))
    )

    def nbExposedSides(allPoints: Set[Point]) = neighbours.count(p => !allPoints(p))
  }

  def parseLine(line: String): Point = {
    val Array(x, y, z) = line.split(",").map(_.toInt)
    Point(x, y, z)
  }
}
