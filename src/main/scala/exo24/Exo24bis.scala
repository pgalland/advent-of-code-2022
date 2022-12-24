package exo24

import java.lang.Math
import scala.collection.mutable

object Exo24bis {
  type Grid = IndexedSeq[IndexedSeq[Tile]]

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo24/input.txt")
    try {
      val grid0     = src.getLines().map(parseLine).toIndexedSeq
      val reachable = mutable.Set.empty[(Int, Int, Int)] // (t, row, column)

      // go to goal
      var time = 0
      var grid = grid0
      reachable.add((0, 0, 1))
      while (!reachable(time, grid0.length - 1, grid0.head.length - 2)) {
        time += 1
        grid = nextGrid(grid)
        grid.indices.foreach(i =>
          grid.head.indices.foreach(j =>
            if (isReachable(time, i, j, reachable, grid)) {
              reachable.add((time, i, j))
            }
          )
        )
      }
      // go back to start
      reachable.clear()
      reachable.add((time, grid.length - 1, grid.head.length - 2))
      while (!reachable(time, 0, 1)) {
        time += 1
        grid = nextGrid(grid)
        grid.indices.foreach(i =>
          grid.head.indices.foreach(j =>
            if (isReachable(time, i, j, reachable, grid)) {
              reachable.add((time, i, j))
            }
          )
        )
      }
      // go back to goal
      reachable.clear()
      reachable.add((time, 0, 1))
      while (!reachable(time, grid0.length - 1, grid0.head.length - 2)) {
        time += 1
        grid = nextGrid(grid)
        grid.indices.foreach(i =>
          grid.head.indices.foreach(j =>
            if (isReachable(time, i, j, reachable, grid)) {
              reachable.add((time, i, j))
            }
          )
        )
      }

      println(time)
    } finally {
      src.close()
    }
  }

  def isReachable(
      time: Int,
      row: Int,
      column: Int,
      reachable: mutable.Set[(Int, Int, Int)],
      grid: Grid
  ): Boolean = {
    {
      grid(row)(column) match
        case Empty => true
        case _     => false
    } && (reachable((time - 1, row, column)) ||
      reachable((time - 1, row - 1, column)) ||
      reachable((time - 1, row + 1, column)) ||
      reachable((time - 1, row, column - 1)) ||
      reachable((time - 1, row, column + 1)))
  }

  def nextGrid(grid: Grid): Grid = {
    val newGrid = grid.indices.map(_ => Array.fill[Tile](grid.head.length)(Empty)).toArray
    (for
      rowIdx    <- grid.indices
      columnIdx <- grid(rowIdx).indices if grid(rowIdx)(columnIdx) == Wall
    yield (rowIdx, columnIdx)).foreach { case (i, j) => newGrid(i)(j) = Wall }

    grid.zipWithIndex
      .flatMap { case (row, i) =>
        row.zipWithIndex.flatMap {
          case (blizzard: Blizzard, j) => blizzard.next(i, j, grid)
          case _                       => Seq.empty
        }
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).reduce { case (b1, b2) => b1.add(b2) })
      .toSeq
      .foreach { case ((row, column), blizzard) =>
        newGrid(row)(column) = blizzard
      }
    newGrid.map(_.toIndexedSeq)
  }

  sealed trait Tile

  case object Wall extends Tile

  case object Empty extends Tile

  case class Blizzard(left: Int = 0, right: Int = 0, down: Int = 0, up: Int = 0) extends Tile {
    def next(row: Int, column: Int, grid: Grid): Seq[((Int, Int), Blizzard)] = {
      Seq(
        if (left > 0) {
          Some(
            (
              row,
              if (grid(row)(column - 1) == Wall) grid.head.length - 2 else column - 1
            ) -> Blizzard(left = left)
          )
        } else {
          None
        },
        if (right > 0) {
          Some(
            (row, if (grid(row)(column + 1) == Wall) 1 else column + 1) -> Blizzard(right = right)
          )
        } else {
          None
        },
        if (down > 0) {
          Some((if (grid(row + 1)(column) == Wall) 1 else row + 1, column) -> Blizzard(down = down))
        } else {
          None
        },
        if (up > 0) {
          Some(
            (if (grid(row - 1)(column) == Wall) grid.length - 2 else row - 1, column) -> Blizzard(
              up = up
            )
          )
        } else {
          None
        }
      ).flatten
    }

    def add(other: Blizzard) = this.copy(
      left = left + other.left,
      right = right + other.right,
      down = down + other.down,
      up = up + other.up
    )
  }

  def parseLine(line: String): IndexedSeq[Tile] = {
    line.toCharArray.map {
      case '#' => Wall
      case '.' => Empty
      case '<' => Blizzard(left = 1)
      case '>' => Blizzard(right = 1)
      case 'v' => Blizzard(down = 1)
      case '^' => Blizzard(up = 1)
    }
  }
}
