package exo21

import java.lang.Math
import scala.collection.mutable

object Exo21bis {

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo21/input.txt")
    try {
      val lines            = src.getLines()
      val monkeys          = lines.map(parseLine).toSeq
      val directMonkeys    = monkeys.flatMap(_._1)
      val operationMonkeys = monkeys.flatMap(_._2)

      val numbers = mutable.Map.empty[String, X | BigInt]
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

  case class X(mult: BigInt, plus: BigInt, div: BigInt) {
    def simplify: X = {
      if (mult % div == 0 && plus % div == 0) {
        X(mult / div, plus / div, if (div > 0) 1 else -1)
      } else {
        this
      }
    }
  }
  case class DirectMonkey(name: String, number: X | BigInt)
  case class OperationMonkey(
      name: String,
      operand1: String,
      operand2: String,
      op: (X | BigInt, X | BigInt) => X | BigInt
  )

  def parseLine(line: String): (Option[DirectMonkey], Option[OperationMonkey]) = {
    val Array(name, rest) = line.split(": ")
    if (name == "humn") {
      Some(DirectMonkey(name, X(mult = 1, plus = 0, div = 1))) -> None
    } else if (rest.forall(_.isDigit)) {
      Some(DirectMonkey(name, BigInt(rest.toLong))) -> None
    } else {
      val Array(op1, op, op2) = rest.split(" ")
      None -> Some(
        OperationMonkey(
          name,
          op1,
          op2,
          (x1: X | BigInt, x2: X | BigInt) =>
            if (name != "root") {
              (x1, x2, op) match
                case (a: BigInt, b: BigInt, "+") => a + b
                case (a: BigInt, b: BigInt, "*") => a * b
                case (a: BigInt, b: BigInt, "-") => a - b
                case (a: BigInt, b: BigInt, "/") => a / b
                case (X(m, p, d), b: BigInt, "+") =>
                  X(m, p + d * b, d).simplify
                case (a: BigInt, X(m, p, d), "+") =>
                  X(m, p + d * a, d).simplify
                case (X(m, p, d), b: BigInt, "-") =>
                  X(m, p - d * b, d).simplify
                case (a: BigInt, X(m, p, d), "-") =>
                  X(-m, d * a - p, d).simplify
                case (X(m, p, d), b: BigInt, "*") =>
                  X(m * b, p * b, d).simplify
                case (a: BigInt, X(m, p, d), "*") =>
                  X(a * m, a * p, d).simplify
                case (X(m, p, d), b: BigInt, "/") =>
                  X(m, p, d * b).simplify
            } else {
              (x1, x2) match
                case (X(m, p, d), b: BigInt) => (d * b - p) / m
                case (a: BigInt, X(m, p, d)) => (d * a - p) / m
            }
        )
      )
    }
  }
}
