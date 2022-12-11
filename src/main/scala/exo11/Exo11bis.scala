package exo11

import scala.collection.mutable

object Exo11bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo11/input.txt")
    try {
      val lines = src.getLines().toSeq
      // Trick: keep worry level modulo the product of dividers.
      val modulo = lines
        .filter(_.startsWith("  Test: divisible by "))
        .map(_.stripPrefix("  Test: divisible by ").toLong)
        .product
      val monkeys = lines
        .grouped(7)
        .map(monkeyLines => parseMonkey(monkeyLines.drop(1).iterator))
        .toArray
      (1 to 10000).foreach { _ =>
        monkeys.foreach(monkey =>
          monkey.turnResult(modulo).foreach { case (destMonkey, item) =>
            monkeys(destMonkey).items.append(item)
          }
        )
      }
      val Array(m1, m2) = monkeys.sortBy(_.nbInspections).reverse.take(2)
      println(m1.nbInspections * m2.nbInspections)
    } finally {
      src.close()
    }
  }

  def parseMonkey(lines: Iterator[String]): Monkey = {
    val items = lines.next().strip().stripPrefix("Starting items: ").split(", ").map(_.toLong)
    val Array(symbol, otherNumber) =
      lines.next().strip().stripPrefix("Operation: new = old ").split(" ")
    val divisibleBy = lines.next().strip().stripPrefix("Test: divisible by ").toLong
    val trueChoice  = lines.next().strip().stripPrefix("If true: throw to monkey ").toInt
    val falseChoice = lines.next().strip().stripPrefix("If false: throw to monkey ").toInt

    Monkey(
      items = mutable.Buffer.from(items),
      operation = (old: Long) =>
        (symbol, otherNumber) match
          case ("+", "old") => old + old
          case ("*", "old") => old * old
          case ("+", _)     => old + otherNumber.toLong
          case ("*", _)     => old * otherNumber.toLong
      ,
      throwTo = (worry: Long) =>
        worry % divisibleBy match
          case 0 => trueChoice
          case _ => falseChoice
    )
  }

  case class Monkey(items: mutable.Buffer[Long], operation: Long => Long, throwTo: Long => Int) {
    var nbInspections: BigInt = BigInt(0)
    def turnResult(modulo: Long): Seq[(Int, Long)] = {
      val forInspection = items.toSeq
      items.clear()
      forInspection.map { worry =>
        nbInspections += 1
        val newWorry = operation(worry) % modulo
        throwTo(newWorry) -> newWorry
      }
    }
  }
}
