package exo12

import scala.collection.mutable

object Exo12 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo12/input.txt")
    try {
      val lines              = src.getLines().toSeq
      val (grid, start, end) = parseGrid(lines)

      val seen     = mutable.Set.from(Seq(start))
      val toVisit  = mutable.Queue.from(Seq(start))
      val distance = mutable.Map.empty[(Int, Int), Int]
      distance.addOne(start, 0)

      while (toVisit.nonEmpty) {
        val current = toVisit.removeHead()
        val next    = choices(current, grid, seen)
        next.foreach(seen.add)
        next.foreach(xy => distance.addOne(xy, distance(current) + 1))
        toVisit.appendAll(next)
      }

      println(distance(end))
    } finally {
      src.close()
    }
  }

  def choices(
      position: (Int, Int),
      grid: Array[Array[Int]],
      seen: mutable.Set[(Int, Int)]
  ): Seq[(Int, Int)] = {
    val (rowNb, columnNb) = position
    Seq(
      (rowNb + 1, columnNb),
      (rowNb - 1, columnNb),
      (rowNb, columnNb + 1),
      (rowNb, columnNb - 1)
    ).filter { case (x, y) => 0 <= x && x < grid.length && 0 <= y && y < grid(0).length }
      .filterNot(seen)
      .filter { case (x, y) => grid(x)(y) <= grid(rowNb)(columnNb) + 1 }
  }

  def parseGrid(lines: Seq[String]): (Array[Array[Int]], (Int, Int), (Int, Int)) = {
    val grid = lines
      .map(line =>
        line.toCharArray.map { c =>
          c match
            case 'S' => 0
            case 'E' => 25
            case _   => c - 97
        }
      )
      .toArray
    val start = lines.zipWithIndex.flatMap { case (line, rowNb) =>
      if (line.indexOf('S') >= 0) Some(rowNb -> line.indexOf('S')) else None
    }.head
    val end = lines.zipWithIndex.flatMap { case (line, rowNb) =>
      if (line.indexOf('E') >= 0) Some(rowNb -> line.indexOf('E')) else None
    }.head

    (grid, start, end)
  }
}
