package exo14

import scala.collection.mutable

object Exo14 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo14/input.txt")
    try {
      val lines     = src.getLines().toSeq
      val rocks     = lines.flatMap(lineToCoordinates)
      val nbRows    = rocks.map(_._1).max + 1
      val nbColumns = rocks.map(_._2).max + 1
      val grid      = (1 to nbRows).map(_ => Array.fill(nbColumns)(false)).toArray
      rocks.foreach { case (x, y) => grid(x)(y) = true }

      var nbGrainsResting = 0
      while (!dropGrainOfSand(position = 0 -> 500, grid)) {
        nbGrainsResting += 1
      }

      println(nbGrainsResting)
    } finally {
      src.close()
    }
  }

  def dropGrainOfSand(position: (Int, Int), grid: Array[Array[Boolean]]): Boolean = {
    val (x, y) = position
    // out of zone
    if (x < 0 || x >= grid.length || y < 0 || y >= grid.head.length) {
      true
    } else if (downFree(position, grid)) {
      dropGrainOfSand((x + 1, y), grid)
    } else if (downLeftFree(position, grid)) {
      dropGrainOfSand((x + 1, y - 1), grid)
    } else if (downRightFree(position, grid)) {
      dropGrainOfSand((x + 1, y + 1), grid)
    } else {
      grid(x)(y) = true
      false
    }
  }

  def downFree(position: (Int, Int), grid: Array[Array[Boolean]]): Boolean = {
    val (x, y) = position
    x + 1 >= grid.length || !grid(x + 1)(y)
  }

  def downLeftFree(position: (Int, Int), grid: Array[Array[Boolean]]): Boolean = {
    val (x, y) = position
    x + 1 >= grid.length || y - 1 < 0 || !grid(x + 1)(y - 1)
  }

  def downRightFree(position: (Int, Int), grid: Array[Array[Boolean]]): Boolean = {
    val (x, y) = position
    x + 1 >= grid.length || y + 1 >= grid.head.length || !grid(x + 1)(y + 1)
  }

  def lineToCoordinates(line: String): Seq[(Int, Int)] = {
    val points = line.split(" -> ").map { p =>
      val Array(col, row) = p.split(",").map(_.toInt)
      row -> col
    }
    points.zip(points.tail).flatMap { case ((x1, y1), (x2, y2)) =>
      if (x1 == x2) {
        (math.min(y1, y2) to math.max(y1, y2)).map(y => x1 -> y)
      } else {
        (math.min(x1, x2) to math.max(x1, x2)).map(x => x -> y1)
      }
    }
  }
}
