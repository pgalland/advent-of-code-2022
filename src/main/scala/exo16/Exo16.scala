package exo16

import scala.collection.mutable

object Exo16 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo16/input.txt")
    try {
      val lines      = src.getLines().toSeq
      val parsed     = lines.map(parseLine)
      val rates      = parsed.map(res => res._1 -> res._2).toMap
      val neighbours = parsed.map(res => res._1 -> res._3).toMap
      val distances = rates.keys
        .flatMap(start => rates.keys.map(end => (start, end) -> distance(start, end, neighbours)))
        .toMap
      val nonZeroRate = rates.filter(_._2 > 0).keySet

      val result = nonZeroRate
        .map(valve =>
          maxRelease(
            distances,
            rates,
            currentValve = valve,
            leftToOpen = nonZeroRate,
            timeLeft = 30 - distances("AA" -> valve)
          )
        )
        .max

      println(result)
    } finally {
      src.close()
    }
  }

  /** I am at current valve to open it and continue. */
  def maxRelease(
      distances: Map[(String, String), Int],
      rates: Map[String, Int],
      currentValve: String,
      leftToOpen: Set[String],
      timeLeft: Int
  ): Int = {
    if (timeLeft <= 0) {
      0
    } else {
      // Open the current valve.
      val release       = (timeLeft - 1) * rates(currentValve)
      val nowLeftToOpen = leftToOpen - currentValve
      // go to either of the next valves
      release + nowLeftToOpen
        .map { nextValve =>
          val timeToGo = distances(currentValve -> nextValve)
          maxRelease(
            distances,
            rates,
            nextValve,
            nowLeftToOpen,
            timeLeft =
              timeLeft - 1 - timeToGo // 1 for opening currentValve, timeToGo for going to nextValve.
          )
        }
        .maxOption
        .getOrElse(0)
    }
  }

  def distance(start: String, end: String, neighbours: Map[String, Seq[String]]): Int = {
    val seen    = mutable.Set.empty[String]
    val toVisit = mutable.Queue.empty[(String, Int)]
    seen.add(start)
    toVisit.append(start -> 0)
    while (!toVisit.map(_._1).contains(end)) {
      val (current, dist) = toVisit.removeHead()
      val next            = neighbours.getOrElse(current, Seq.empty).filterNot(seen)
      seen.addAll(next)
      toVisit.appendAll(next.map(_ -> (dist + 1)))
    }

    toVisit.find(_._1 == end).get._2
  }

  type Path = Seq[Action]
  sealed trait Action {
    def name: String
  }
  case class Open(name: String)                 extends Action
  case class MoveTo(name: String, from: String) extends Action

  /** @return (valve, rate, tunnel destinations) */
  def parseLine(line: String): (String, Int, Seq[String]) = {
    val Array(Array(valve, rate), valves) = line
      .stripPrefix("Valve ")
      .replace(" has flow rate=", ",")
      .replace(" tunnels lead to valves ", "")
      .replace(" tunnel leads to valve ", "")
      .split(";")
      .map(_.split(","))
    (valve.strip(), rate.toInt, valves.map(_.strip()).toSeq)
  }
}
