package exo22

import java.lang.Math
import scala.collection.mutable

object Exo22 {
  import Tile._
  import Direction._
  type Grid = Array[Array[Tile]]

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo22/input.txt")
    try {
      val lines             = src.getLines().toSeq
      val tempGrid          = lines.slice(0, lines.length - 2).map(parseGridLine).toArray
      val grid              = tempGrid.map(row => padRow(length = tempGrid.map(_.length).max, row))
      val moves             = parseMovesLine(lines.last)
      val startingPos       = grid.head.indexWhere(_ == Open) -> 0
      val startingDirection = Right
      val ((finalX, finalY), finalDirection) = moves.foldLeft(startingPos -> startingDirection) {
        case ((pos, direction), move) =>
          applyMove(pos, direction, move, grid)
      }
      val result = 1000 * (finalY + 1) + 4 * (finalX + 1) + finalDirection.ordinal

      println(result)
    } finally {
      src.close()
    }
  }

  def parseGridLine(line: String): Array[Tile] =
    line.toCharArray.map {
      case ' ' => Nothing
      case '.' => Open
      case '#' => Wall
    }

  def padRow(length: Int, row: Array[Tile]): Array[Tile] = {
    if (row.length < length) row ++ Array.fill(length - row.length)(Nothing) else row
  }

  def parseMovesLine(line: String): Seq[Move] = {
    val numbers = line.split("L|R").map(_.toInt).map(Advance.apply)
    val turns = line
      .split("[0123456789]+")
      .filterNot(_ == "")
      .map {
        case "R" => Turn90Clockwise
        case "L" => Turn90AntiClockwise
      }
    numbers.zip(turns).flatMap { case (n, turn) => Seq(n, turn) } :+ numbers.last
  }

  def applyMove(
      pos: (Int, Int),
      direction: Direction,
      move: Move,
      grid: Grid
  ): ((Int, Int), Direction) = {
    move match
      case Turn90Clockwise =>
        pos -> Direction.fromOrdinal(Math.floorMod(direction.ordinal + 1, Direction.values.length))
      case Turn90AntiClockwise =>
        pos -> Direction.fromOrdinal(Math.floorMod(direction.ordinal - 1, Direction.values.length))
      case Advance(0) => pos -> direction
      case Advance(n) => applyMove(nextPos(pos, direction, grid), direction, Advance(n - 1), grid)
  }

  def nextPos(pos: (Int, Int), direction: Direction, grid: Grid): (Int, Int) = {
    def wrapRow(a: Int, b: Int): (Int, Int) = {
      val row       = grid(b)
      val startRow  = row.indexWhere(_ != Nothing)
      val rowLength = row.count(_ != Nothing)
      val aa        = startRow + Math.floorMod(a - startRow, rowLength)

      aa -> b
    }
    def wrapColumn(a: Int, b: Int): (Int, Int) = {
      val column       = grid.indices.map(i => grid(i)(a))
      val startColumn  = column.indexWhere(_ != Nothing)
      val columnLength = column.count(_ != Nothing)
      val bb           = startColumn + Math.floorMod(b - startColumn, columnLength)

      a -> bb
    }

    val (x, y) = pos
    val (newX, newY) = direction match
      case Right => wrapRow(x + 1, y)
      case Left  => wrapRow(x - 1, y)
      case Down  => wrapColumn(x, y + 1)
      case Up    => wrapColumn(x, y - 1)
    if (grid(newY)(newX) == Wall) pos else newX -> newY
  }

  trait Move
  case class Advance(n: Int)      extends Move
  case object Turn90Clockwise     extends Move
  case object Turn90AntiClockwise extends Move

  enum Tile:
    case Wall, Open, Nothing

  enum Direction:
    case Right, Down, Left, Up
}
