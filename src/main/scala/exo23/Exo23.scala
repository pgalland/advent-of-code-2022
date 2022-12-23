package exo23

import java.lang.Math
import scala.collection.mutable

object Exo23 {
  import Case.*
  import Direction.*
  type Grid     = Array[Array[Case]]
  type Position = (Int, Int)

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo23/input.txt")
    try {
      val lines               = src.getLines().toArray
      val grid                = padGrid(lines.map(parseLine), padding = 20)
      val initDirectionGroups = Seq(Seq(N, NW, NE), Seq(S, SW, SE), Seq(W, NW, SW), Seq(E, NE, SE))

      val _ = (1 to 10).foldLeft(initDirectionGroups) { case (directionGroups, _) =>
        applyNewPositions(grid, filterConflicts(makePropositions(grid, directionGroups)))
        directionGroups.tail :+ directionGroups.head
      }
      val result = countOpenSpots(grid)

      println(result)
    } finally {
      src.close()
    }
  }

  def elvePositions(grid: Grid): Seq[Position] = {
    for
      y <- grid.indices
      x <- grid(y).indices if grid(x, y) == Elf
    yield (x, y)
  }

  def makePropositions(
      grid: Grid,
      directionGroups: Seq[Seq[Direction]]
  ): Map[Position, Position] = {
    elvePositions(grid).flatMap { case (x, y) =>
      if (Direction.values.forall(_.isEmpty(grid, x, y)))
        None
      else
        directionGroups
          .find(_.forall(_.isEmpty(grid, x, y)))
          .map(_.head.move(x, y))
          .map((x, y) -> _)
    }.toMap
  }

  def filterConflicts(propositions: Map[Position, Position]): Map[Position, Position] = {
    propositions.toSeq.groupBy(_._2).filter(_._2.length == 1).map(_._2.head)
  }

  def applyNewPositions(grid: Grid, newPositions: Map[Position, Position]) = {
    newPositions.foreach { case ((x, y), (newX, newY)) =>
      grid(y)(x) = Empty
      grid(newY)(newX) = Elf
    }
  }

  def countOpenSpots(grid: Grid): Int = {
    val positions = elvePositions(grid)
    val minX      = positions.map(_._1).min
    val maxX      = positions.map(_._1).max
    val minY      = positions.map(_._2).min
    val maxY      = positions.map(_._2).max
    (for
      x <- minX to maxX
      y <- minY to maxY if grid(x, y) == Empty
    yield 1).sum
  }

  enum Direction(val dx: Int, val dy: Int):
    case N  extends Direction(0, -1)
    case NE extends Direction(1, -1)
    case E  extends Direction(1, 0)
    case SE extends Direction(1, 1)
    case S  extends Direction(0, 1)
    case SW extends Direction(-1, 1)
    case W  extends Direction(-1, 0)
    case NW extends Direction(-1, -1)

  enum Case:
    case Elf, Empty

  def parseLine(line: String) = line.toCharArray.map {
    case '.' => Empty
    case '#' => Elf
  }

  def padGrid(grid: Grid, padding: Int): Grid = {
    (0 until padding).map(_ => Array.fill(grid.head.length + 2 * padding)(Empty)).toArray ++
      grid.map(row => Array.fill(padding)(Empty) ++ row ++ Array.fill(padding)(Empty)) ++
      (0 until padding).map(_ => Array.fill(grid.head.length + 2 * padding)(Empty)).toArray
  }

  extension (g: Grid) def apply(x: Int, y: Int) = g(y)(x)

  extension (d: Direction) {
    def isEmpty(g: Grid, x: Int, y: Int) = g(x + d.dx, y + d.dy) == Empty
    def move(x: Int, y: Int): Position   = (x + d.dx, y + d.dy)
  }
}
