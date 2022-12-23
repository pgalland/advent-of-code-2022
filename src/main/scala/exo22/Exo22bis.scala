package exo22

import java.lang.Math
import scala.collection.mutable

object Exo22bis {

  import Direction.*
  import Tile.*
  import Side.*

  type Grid  = Array[Array[Tile]]
  type Grids = Array[Grid]

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo22/input.txt")
    try {
      val lines    = src.getLines().toSeq
      val tempGrid = lines.slice(0, lines.length - 2).map(parseGridLine).toArray
      val grid     = tempGrid.map(row => padRow(length = tempGrid.map(_.length).max, row))
      val moves    = parseMovesLine(lines.last)

      /* Test input.
      val squareLength = 4
      val gridsTopLeft = Array(8 -> 0, 0 -> 4, 4 -> 4, 8 -> 4, 8 -> 8, 12 -> 8)
      val gridTransitions = Map(
        0 -> Seq(5 -> RightSide, 3 -> Top, 2 -> Top, 1 -> Top),
        1 -> Seq(2 -> LeftSide, 4 -> Bottom, 5 -> Bottom, 0 -> Top),
        2 -> Seq(3 -> LeftSide, 4 -> LeftSide, 1 -> RightSide, 0 -> LeftSide),
        3 -> Seq(5 -> Top, 4 -> Top, 2 -> RightSide, 0 -> Bottom),
        4 -> Seq(5 -> LeftSide, 1 -> Bottom, 2 -> Bottom, 3 -> Bottom),
        5 -> Seq(0 -> RightSide, 1 -> Bottom, 4 -> RightSide, 3 -> RightSide)
      )
       */
      val squareLength = 50
      val gridsTopLeft = Array(50 -> 0, 100 -> 0, 50 -> 50, 0 -> 100, 50 -> 100, 0 -> 150)
      val gridTransitions = Map(
        0 -> Seq(1 -> LeftSide, 2 -> Top, 3 -> LeftSide, 5 -> LeftSide),
        1 -> Seq(4 -> RightSide, 2 -> RightSide, 0 -> RightSide, 5 -> Bottom),
        2 -> Seq(1 -> Bottom, 4 -> Top, 3 -> Top, 0 -> Bottom),
        3 -> Seq(4 -> LeftSide, 5 -> Top, 0 -> LeftSide, 2 -> LeftSide),
        4 -> Seq(1 -> RightSide, 5 -> RightSide, 3 -> RightSide, 2 -> Bottom),
        5 -> Seq(4 -> Bottom, 1 -> Top, 0 -> Top, 3 -> Bottom)
      )

      val grids = gridsTopLeft.map(extractGrid(grid, squareLength))

      val startingPos = Position(0, 0, 0, Right)
      val Position(gridNumber, finalX, finalY, finalDirection) = moves.foldLeft(startingPos) {
        case (currentPosition, move) =>
          applyMove(currentPosition, move, grids, gridTransitions)
      }
      val (gridX, gridY) = gridsTopLeft(gridNumber)
      val result = 1000 * (gridY + finalY + 1) + 4 * (gridX + finalX + 1) + finalDirection.ordinal

      println(result)

    } finally {
      src.close()
    }
  }

  def applyMove(
      pos: Position,
      move: Move,
      grids: Grids,
      gridTransition: Map[Int, Seq[(Int, Side)]]
  ): Position = {
    val max = grids.head.length - 1
    move match
      case Turn90Clockwise =>
        pos.copy(direction =
          Direction.fromOrdinal(Math.floorMod(pos.direction.ordinal + 1, Direction.values.length))
        )
      case Turn90AntiClockwise =>
        pos.copy(direction =
          Direction.fromOrdinal(Math.floorMod(pos.direction.ordinal - 1, Direction.values.length))
        )
      case Advance(0) => pos
      case Advance(n) =>
        val (newGrid, destSide) = gridTransition(pos.gridNumber)(pos.direction.ordinal)
        val newPos = (pos.direction, pos.x, pos.y) match
          case (Right, x, _) if x == max =>
            sideTransition(max, newGrid, RightSide, destSide, pos.x, pos.y)
          case (Left, 0, _) => sideTransition(max, newGrid, LeftSide, destSide, pos.x, pos.y)
          case (Up, _, 0)   => sideTransition(max, newGrid, Top, destSide, pos.x, pos.y)
          case (Down, _, y) if y == max =>
            sideTransition(max, newGrid, Bottom, destSide, pos.x, pos.y)
          case (Right, x, _) => pos.copy(x = x + 1)
          case (Left, x, _)  => pos.copy(x = x - 1)
          case (Up, _, y)    => pos.copy(y = y - 1)
          case (Down, _, y)  => pos.copy(y = y + 1)

        if (newPos.isWall(grids)) pos else applyMove(newPos, Advance(n - 1), grids, gridTransition)
  }

  def sideTransition(
      max: Int,
      newGrid: Int,
      fromSide: Side,
      destSide: Side,
      x: Int,
      y: Int
  ): Position = {
    val newDirection = destSide match
      case RightSide => Left
      case LeftSide  => Right
      case Top       => Down
      case Bottom    => Up

    val (newX, newY) = (fromSide, destSide) match
      case (LeftSide, RightSide)  => (max, y)
      case (RightSide, LeftSide)  => (0, y)
      case (RightSide, Top)       => (max - y, 0)
      case (Top, RightSide)       => (max, x)
      case (Top, Bottom)          => (x, max)
      case (Bottom, Top)          => (x, 0)
      case (RightSide, RightSide) => (max, max - y)
      case (LeftSide, LeftSide)   => (0, max - y)
      case (Bottom, Bottom)       => (max - x, max)
      case (Top, LeftSide)        => (0, x)
      case (RightSide, Bottom)    => (y, max)
      case (Bottom, RightSide)    => (max, x)
      case (LeftSide, Top)        => (y, 0)

    Position(newGrid, newX, newY, newDirection)
  }

  case class Position(gridNumber: Int, x: Int, y: Int, direction: Direction) {
    def isWall(grids: Grids): Boolean = {
      grids(gridNumber)(y)(x) == Wall
    }
  }

  trait Move

  case class Advance(n: Int) extends Move

  case object Turn90Clockwise extends Move

  case object Turn90AntiClockwise extends Move

  enum Tile:
    case Wall, Open, Nothing

  enum Direction:
    case Right, Down, Left, Up

  enum Side:
    case Top, RightSide, Bottom, LeftSide

  def padRow(length: Int, row: Array[Tile]): Array[Tile] = {
    if (row.length < length) row ++ Array.fill(length - row.length)(Nothing) else row
  }

  def parseGridLine(line: String): Array[Tile] =
    line.toCharArray.map {
      case ' ' => Nothing
      case '.' => Open
      case '#' => Wall
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

  def extractGrid(fullGrid: Grid, squareLength: Int)(x: Int, y: Int): Grid = {
    fullGrid.slice(y, y + squareLength).map(_.slice(x, x + squareLength))
  }
}
