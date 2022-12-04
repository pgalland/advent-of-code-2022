package exo2

import Exo2bis.Chifumi._
import Exo2bis.Outcome._

object Exo2bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo2/input.txt")
    try {
      val lines = src.getLines()
      val score = lines.map { line =>
        val tokens       = line.split(" ")
        val opponentPlay = parsePlay(tokens(0))
        val outcome      = parseOutcome(tokens(1))
        val myPlay       = findPlay(opponentPlay, outcome)
        roundScore(myPlay = myPlay, opponentPlay = opponentPlay).toLong
      }.sum
      println(s"Total score: $score")
    } finally {
      src.close()
    }
  }

  def parsePlay(letter: String) = letter match {
    case "A" => Rock
    case "B" => Paper
    case "C" => Scissors
  }

  def parseOutcome(letter: String) = letter match {
    case "X" => Lose
    case "Y" => Draw
    case "Z" => Win
  }

  def findPlay(opponentPlay: Chifumi, outcome: Outcome): Chifumi = {
    (opponentPlay, outcome) match
      case (Rock, Lose)     => Scissors
      case (Rock, Win)      => Paper
      case (Paper, Lose)    => Rock
      case (Paper, Win)     => Scissors
      case (Scissors, Lose) => Paper
      case (Scissors, Win)  => Rock
      case (_, Draw)        => opponentPlay
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

  enum Outcome:
    case Lose, Draw, Win
}
