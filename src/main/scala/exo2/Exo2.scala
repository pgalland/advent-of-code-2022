package exo2

import Exo2.Chifumi._

object Exo2 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo2/input.txt")
    try {
      val lines = src.getLines()
      val score = lines.map { line =>
        val tokens = line.split(" ")
        roundScore(myPlay = parse(tokens(1)), opponentPlay = parse(tokens(0))).toLong
      }.sum
      println(s"Total score: $score")
    } finally {
      src.close()
    }
  }

  def parse(letter: String) = letter match {
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissors
  }

  def roundScore(myPlay: Chifumi, opponentPlay: Chifumi): Int = {
    val soloScore = myPlay match
      case Rock     => 1
      case Paper    => 2
      case Scissors => 3

    val outcomeScore = (myPlay, opponentPlay) match
      case (Rock, Scissors)     => 6
      case (Rock, Rock)         => 3
      case (Rock, Paper)        => 0
      case (Paper, Rock)        => 6
      case (Paper, Paper)       => 3
      case (Paper, Scissors)    => 0
      case (Scissors, Paper)    => 6
      case (Scissors, Scissors) => 3
      case (Scissors, Rock)     => 0

    soloScore + outcomeScore
  }

  enum Chifumi:
    case Rock, Paper, Scissors
}
