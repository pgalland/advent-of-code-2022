package exo23

import java.lang.Math
import scala.collection.mutable

object Exo23bis {

  import Case.*
  import Direction.*

  type Position = (Int, Int)

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo23/input.txt")
    try {
      val lines = src.getLines().toArray
      val grid  = new Grid(lines.map(parseLine))

      var result          = 0
      var movesNeeded     = true
      var directionGroups = Seq(Seq(N, NW, NE), Seq(S, SW, SE), Seq(W, NW, SW), Seq(E, NE, SE))
      while (movesNeeded) {
        result += 1
        if (allGood(grid)) {
          movesNeeded = false
        } else {
          applyNewPositions(grid, filterConflicts(makePropositions(grid, directionGroups)))
          directionGroups = directionGroups.tail :+ directionGroups.head
        }
      }

      println(result)
    } finally {
      src.close()
    }
  }

  def allGood(grid: Grid): Boolean =
    grid.allElvesPositions.forall { case (x, y) => Direction.values.forall(_.isEmpty(grid, x, y)) }

  def makePropositions(
      grid: Grid,
      directionGroups: Seq[Seq[Direction]]
  ): Map[Position, Position] = {
    grid.allElvesPositions.flatMap { case (x, y) =>
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
      grid.clear(x, y)
      grid.addElve(newX, newY)
    }
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
    case Elve, Empty

  def parseLine(line: String) = line.toCharArray.map {
    case '.' => Empty
    case '#' => Elve
  }

  extension (d: Direction) {
    def isEmpty(g: Grid, x: Int, y: Int) = g.isEmpty(x + d.dx, y + d.dy)
    def move(x: Int, y: Int): Position   = (x + d.dx, y + d.dy)
  }

  class Grid(private val parsedGrid: Array[Array[Case]]) {
    private val elvePositions = mutable.Set.from({
      for
        y <- parsedGrid.indices
        x <- parsedGrid(y).indices if parsedGrid(y)(x) == Elve
      yield (x, y)
    })

    def allElvesPositions: Set[Position] = elvePositions.toSet

    def isEmpty(x: Int, y: Int): Boolean = !elvePositions.contains(x -> y)

    def clear(x: Int, y: Int): Unit   = elvePositions.remove(x -> y)
    def addElve(x: Int, y: Int): Unit = elvePositions.add(x -> y)
  }
}
