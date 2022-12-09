package exo9

import java.nio.file.{Path, Paths}
import scala.collection.{BufferedIterator, mutable}

object Exo9bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo9/input.txt")
    try {
      val lines = src.getLines()
      val moves = lines
        .flatMap { line =>
          val Array(direction, nbMoves) = line.split(" ")
          (1 to nbMoves.toInt).map(_ => direction)
        }
        .map(parseMove)

      val (visited, _) = moves.foldLeft(Set(0 -> 0) -> Seq.fill(10)(0 -> 0)) {
        case ((visitedByTail, positions), moveFunc) =>
          val newHead = moveFunc(positions.head)
          val newPositionsComplete = positions.tail.foldLeft(Seq(newHead)) {
            case (newPositions, posNplus1) =>
              val newPosNplus1 = follow(posNplus1, newPositions.last)
              newPositions :+ newPosNplus1
          }
          (visitedByTail + newPositionsComplete.last) -> newPositionsComplete
      }
      println(visited.size)
    } finally {
      src.close()
    }
  }

  def follow(posNplus1: (Int, Int), newPosN: (Int, Int)): (Int, Int) = {
    val shift = (newPosN._1 - posNplus1._1, newPosN._2 - posNplus1._2) match
      case (1, 2) | (2, 2) | (2, 1)       => (1, 1)
      case (2, -1) | (2, -2) | (1, -2)    => (1, -1)
      case (-1, -2) | (-2, -2) | (-2, -1) => (-1, -1)
      case (-2, 1) | (-2, 2) | (-1, 2)    => (-1, 1)
      case (2, 0)                         => (1, 0)
      case (0, 2)                         => (0, 1)
      case (0, -2)                        => (0, -1)
      case (-2, 0)                        => (-1, 0)
      case _                              => (0, 0)

    (posNplus1._1 + shift._1, posNplus1._2 + shift._2)
  }

  def parseMove(direction: String): ((Int, Int)) => (Int, Int) = {
    val shift = direction match
      case "R" => (1, 0)
      case "L" => (-1, 0)
      case "U" => (0, 1)
      case "D" => (0, -1)
    (head: (Int, Int)) => (head._1 + shift._1, head._2 + shift._2)
  }
}
