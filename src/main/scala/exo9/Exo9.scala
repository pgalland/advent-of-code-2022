package exo9

import java.nio.file.{Path, Paths}
import scala.collection.mutable
import scala.collection.BufferedIterator
object Exo9 {
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
      val (visited, _) = moves.foldLeft(Set.empty[(Int, Int)] -> (0 -> 0, 0 -> 0)) {
        case ((visitedByTail, (tail, head)), moveFunc) =>
          val (newTail, newHead) = moveFunc(tail, head)
          (visitedByTail + newTail) -> (newTail -> newHead)
      }
      println(visited.size)
    } finally {
      src.close()
    }
  }

  def moveRight(tail: (Int, Int), head: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val (xTail, yTail) = tail
    val (xHead, yHead) = head

    //   .HT. for instance
    if (yTail >= yHead) {
      tail -> (xHead, yHead + 1)
    } else { // .TH. or diagonal
      head -> (xHead, yHead + 1)
    }
  }

  def moveLeft(tail: (Int, Int), head: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val (xTail, yTail) = tail
    val (xHead, yHead) = head

    //   .HT. for instance
    if (yTail <= yHead) {
      tail -> (xHead, yHead - 1)
    } else { // .HT. or diagonal
      head -> (xHead, yHead - 1)
    }
  }

  def moveUp(tail: (Int, Int), head: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val (xTail, yTail) = tail
    val (xHead, yHead) = head

    //   .T.
    //   .H.   for instance
    if (xTail >= xHead) {
      tail -> (xHead + 1, yHead)
    } else {
      //  .H.
      //  T..  for instance
      head -> (xHead + 1, yHead)
    }
  }

  def moveDown(tail: (Int, Int), head: (Int, Int)): ((Int, Int), (Int, Int)) = {
    val (xTail, yTail) = tail
    val (xHead, yHead) = head

    //   .H.
    //   .T.   for instance
    if (xTail <= xHead) {
      tail -> (xHead - 1, yHead)
    } else {
      //  .T.
      //  H..  for instance
      head -> (xHead - 1, yHead)
    }
  }

  def parseMove(direction: String) = {
    direction match
      case "R" => moveRight
      case "L" => moveLeft
      case "U" => moveUp
      case "D" => moveDown
  }
}
