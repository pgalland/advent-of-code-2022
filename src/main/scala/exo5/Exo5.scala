package exo5

import scala.collection.mutable

object Exo5 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo5/input.txt")
    try {
      val lines            = src.getLines().toSeq
      val nbLinesForCrates = lines.indexWhere(_.isEmpty)
      val stacks           = parseCrates(lines.take(nbLinesForCrates))
      val moves = parseMoves(lines.drop(nbLinesForCrates + 1))
        .map { case (quantity, from, to) => (quantity, from - 1, to - 1) } // index from zero.

      moves.foreach { case (quantity, from, to) =>
        (1 to quantity).foreach { _ =>
          val crate = stacks(from).pop()
          stacks(to).push(crate)
        }
      }

      println(stacks.map(_.pop()).mkString)
    } finally {
      src.close()
    }
  }

  def parseCrates(lines: Seq[String]): Array[mutable.Stack[String]] = {
    val nbStacks = lines.last.split(" +").last.toInt
    val stacks   = (1 to nbStacks).map(_ => mutable.Stack.empty[String])
    lines
      .dropRight(1)
      .reverse // reverse is important here, we want to push the crates starting from the bottom.
      .foreach { line =>
        line.toCharArray
          .map(_.toString)
          .grouped(4)
          .map(forOneStack => if (forOneStack(1) != " ") Some(forOneStack(1)) else None)
          .zipWithIndex
          .foreach { case (crate, stackIdx) =>
            crate.foreach(crateId => stacks(stackIdx).push(crateId))
          }
      }
    stacks.toArray
  }

  def parseMoves(lines: Seq[String]): Seq[(Int, Int, Int)] = {
    lines.map { line =>
      val tokens = line
        .replace("move ", "")
        .replace("from ", "")
        .replace("to ", "")
        .split(" ")
      (tokens(0).toInt, tokens(1).toInt, tokens(2).toInt)
    }
  }
}
