package exo25

import java.lang.Math
import scala.collection.mutable

object Exo25 {

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo25/input.txt")
    try {
      val numbers = src.getLines().map(parseLine)
      val result  = printSnafu(longToSnafu(numbers.map(snafuToLong).sum))

      println(result)
    } finally {
      src.close()
    }
  }

  def longToSnafu(x: Long): Seq[Int] = {
    var maxK = 0
    while (math.pow(5, maxK) <= x) { maxK += 1 }
    (0 until maxK)
      .foldLeft(Seq.empty[Int] -> x) { case ((rep, rest), _) =>
        val exp = Math.floorMod(rest, 5) match
          case 4 => -1
          case 3 => -2
          case x => x

        (exp +: rep) -> (rest - exp) / 5
      }
      ._1
  }

  def snafuToLong(rep: Seq[Int]): Long = {
    rep.reverse.zipWithIndex.map { case (digit, power) => digit * math.pow(5, power).toLong }.sum
  }

  def parseLine(line: String): Seq[Int] = {
    line.toCharArray.map {
      case '0' => 0
      case '1' => 1
      case '2' => 2
      case '-' => -1
      case '=' => -2
    }
  }

  def printSnafu(snafu: Seq[Int]): String = {
    snafu.map {
      case -2 => '='
      case -1 => '-'
      case 0  => '0'
      case 1  => '1'
      case 2  => '2'
    }.mkString
  }
}
