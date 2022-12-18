package exo17

import scala.collection.mutable

object Exo17bis {

  import Direction.*
  import Shape.*

  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo17/input.txt")
    try {
      val flow = parseLine(src.getLines().next())

      val cycle = findCycle(allTurns(flow, nbTurns = 2022))

      val nbRocks                      = 1000000000000L
      val nbRocksFromCycleStart        = nbRocks - (cycle.rockNbStart + 1)
      val yAtCycleStart                = cycle.yStart
      val nbCycles                     = nbRocksFromCycleStart / cycle.length
      val yGainAllCycles               = nbCycles * cycle.fullYGain
      val remainingRocksAfterAllCycles = nbRocksFromCycleStart % cycle.length
      val yGainRemainingRocks          = cycle.gainAfterNRocks(remainingRocksAfterAllCycles.toInt)
      val result                       = yAtCycleStart + yGainAllCycles + yGainRemainingRocks

      println(result)
    } finally {
      src.close()
    }
  }

  case class Cycle(rockNbStart: Int, yStart: Int, yGains: Seq[Int]) {
    def length: Int = yGains.length

    def fullYGain: Int = yGains.last

    def gainAfterNRocks(n: Int) = yGains(n - 1)
  }

  case class Turn(rockNb: Int, flowIdx: Int, skyline: Seq[Int], maxY: Int)

  def findCycle(turns: Array[Turn]): Cycle = {
    val cycleTurns = turns
      .groupBy(turn => (turn.rockNb % 5, turn.flowIdx, turn.skyline))
      .values
      .filter(_.length >= 2)
      .minBy(_.map(_.rockNb).min)
      .toSeq
      .sortBy(_.rockNb)

    val Turn(rockNbStart, _, _, maxYStart) = cycleTurns.head
    val cycleLength                        = cycleTurns(1).rockNb - rockNbStart
    Cycle(
      rockNbStart = rockNbStart,
      yStart = maxYStart,
      yGains = turns
        .slice(from = rockNbStart + 1, until = rockNbStart + 1 + cycleLength)
        .map(_.maxY - maxYStart)
        .toArray
    )
  }

  def allTurns(flow: Array[Direction], nbTurns: Int): Array[Turn] = {
    val points       = mutable.Set.empty[(Int, Int)]
    val infoByTurn   = mutable.Buffer.empty[Turn]
    var flowIdx: Int = 0

    (0 until nbTurns).foreach { nb =>
      val shape                       = Shape.fromOrdinal(nb % 5)
      var struct                      = Struct(shape, maxY(points))
      var finalStruct: Option[Struct] = None
      while (finalStruct.isEmpty) {
        val direction = flow(flowIdx % flow.length)
        flowIdx += 1
        val translated = struct.translate(direction)
        struct = if (translated.isValid(points)) translated else struct
        val down = struct.down
        if (down.isValid(points)) {
          struct = down
        } else {
          finalStruct = Some(struct)
        }
      }
      points.addAll(finalStruct.get.points)
      val turn = Turn(
        rockNb = nb,
        flowIdx = flowIdx % flow.length,
        skyline = relief(points),
        maxY = maxY(points)
      )
      infoByTurn.append(turn)
    }
    infoByTurn.toArray
  }

  def maxY(points: mutable.Set[(Int, Int)]): Int = points.map(_._2).maxOption.getOrElse(0)

  def relief(points: mutable.Set[(Int, Int)]): Seq[Int] = {
    val heights = points.groupBy(_._1).view.mapValues(_.map(_._2).max)
    val full    = (0 until 7).map(x => heights.getOrElse(x, 0))
    val minY    = full.min
    full.map(_ - minY)
  }

  enum Shape:
    case Flat, Cross, ReverseL, I, Square

  enum Direction:
    case Left, Right

  case class Struct(x: Int, y: Int, shape: Shape) {

    def points: Seq[(Int, Int)] = shape match
      case Flat     => (0 to 3).map(d => (x + d, y))
      case Cross    => Seq((x + 1, y), (x + 1, y - 2)) ++ (0 to 2).map(d => (x + d, y - 1))
      case ReverseL => (0 to 2).map(d => (x + d, y - 2)) ++ Seq((x + 2, y), (x + 2, y - 1))
      case I        => (0 to 3).map(d => (x, y - d))
      case Square   => (0 to 1).flatMap(dx => (0 to 1).map(dy => (x + dx, y - dy)))

    def down: Struct = Struct(x, y - 1, shape)

    def translate(direction: Direction): Struct = direction match
      case Left  => Struct(x - 1, y, shape)
      case Right => Struct(x + 1, y, shape)

    def isValid(filledPoints: mutable.Set[(Int, Int)]): Boolean = {
      points.forall { case (xx, yy) => 0 <= xx && xx < 7 && 1 <= yy && !filledPoints(xx -> yy) }
    }
  }

  object Struct {
    def apply(shape: Shape, maxY: Int): Struct = {
      shape match
        case Flat     => Struct(2, maxY + 4, shape)
        case Cross    => Struct(2, maxY + 6, shape)
        case ReverseL => Struct(2, maxY + 6, shape)
        case I        => Struct(2, maxY + 7, shape)
        case Square   => Struct(2, maxY + 5, shape)
    }
  }

  def parseLine(line: String): Array[Direction] = {
    line.toCharArray.map {
      case '>' => Right
      case '<' => Left
    }
  }
}
