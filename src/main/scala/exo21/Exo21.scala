package exo21

import java.lang.Math
import scala.collection.mutable

object Exo21 {

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo21/input.txt")
    try {
      val lines            = src.getLines()
      val monkeys          = lines.map(parseLine).toSeq
      val directMonkeys    = monkeys.flatMap(_._1)
      val operationMonkeys = monkeys.flatMap(_._2)

      val numbers = mutable.Map.empty[String, Long]
      directMonkeys.foreach(monkey => numbers.update(monkey.name, monkey.number))

      val monkeysLeftToCompute = mutable.Set.from(operationMonkeys)
      while (monkeysLeftToCompute.nonEmpty) {
        monkeysLeftToCompute.iterator.foreach { monkey =>
          if (numbers.contains(monkey.operand1) && numbers.contains(monkey.operand2)) {
            numbers.update(
              monkey.name,
              monkey.op(numbers(monkey.operand1), numbers(monkey.operand2))
            )
            monkeysLeftToCompute.remove(monkey)
          }
        }
      }

      println(numbers("root"))
    } finally {
      src.close()
    }
  }

  case class DirectMonkey(name: String, number: Long)
  case class OperationMonkey(
      name: String,
      operand1: String,
      operand2: String,
      op: (Long, Long) => Long
  )

  def parseLine(line: String): (Option[DirectMonkey], Option[OperationMonkey]) = {
    val Array(name, rest) = line.split(": ")
    if (rest.forall(_.isDigit)) {
      Some(DirectMonkey(name, rest.toLong)) -> None
    } else {
      val Array(op1, op, op2) = rest.split(" ")
      None -> Some(
        OperationMonkey(
          name,
          op1,
          op2,
          (x1: Long, x2: Long) =>
            op match
              case "+" => x1 + x2
              case "*" => x1 * x2
              case "/" => x1 / x2
              case "-" => x1 - x2
        )
      )
    }
  }
}
