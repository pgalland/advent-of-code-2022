package exo18

import scala.collection.mutable

object Exo18bis {

  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo18/input.txt")
    try {
      val lines     = src.getLines()
      val points    = lines.map(parseLine).toSeq
      val allPoints = points.toSet

      val maxX = points.map(_.x).max + 1
      val minX = points.map(_.x).min - 1
      val maxY = points.map(_.y).max + 1
      val minY = points.map(_.y).min - 1
      val maxZ = points.map(_.z).max + 1
      val minZ = points.map(_.z).min - 1

      val components = mutable.Map.empty[Point, Set[Point]]
      (for
        x <- minX to maxX
        y <- minY to maxY
        z <- minZ to maxZ
      yield Point(x, y, z)).foreach { p =>
        if (!allPoints(p)) {
          val airNeighbours = p.neighbours.filterNot(allPoints)
          val newComp =
            (airNeighbours :+ p).map(q => components.getOrElse(q, Set(q))).reduce(_ ++ _)
          (airNeighbours :+ p).foreach(q => components.update(q, newComp))
        }
      }
      val outerPoint     = Point(maxX, maxY, maxZ)
      val exteriorPoints = components(outerPoint)

      val result = points.map(_.nbExposedSides(allPoints, exteriorPoints)).sum

      println(result)
    } finally {
      src.close()
    }
  }

  case class Point(x: Int, y: Int, z: Int) {
    def neighbours: Seq[Point] = Seq(-1, 1).flatMap(shift =>
      Seq(copy(x = x + shift), copy(y = y + shift), copy(z = z + shift))
    )

    def nbExposedSides(allPoints: Set[Point], exteriorPoints: Set[Point]) =
      neighbours.count(p => !allPoints(p) && exteriorPoints(p))
  }

  def parseLine(line: String): Point = {
    val Array(x, y, z) = line.split(",").map(_.toInt)
    Point(x, y, z)
  }
}
