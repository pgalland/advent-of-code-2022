package exo15

import scala.collection.mutable

object Exo15 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo15/input.txt")
    try {
      val lines            = src.getLines().toSeq
      val sensorsToBeacons = lines.map(parseLine).toMap
      val sensorRadius     = sensorsToBeacons.map { case (s, b) => s -> dist(s, b) }
      val sensors          = sensorsToBeacons.keySet
      val beacons          = sensorsToBeacons.values.toSet

      val leftmost  = sensors.map(_._1).min - sensorRadius.values.max - 10
      val rightmost = sensors.map(_._1).max + sensorRadius.values.max + 10

      val result = (leftmost to rightmost).count { x =>
        val pos = x -> 2000000
        !sensors(pos) && !beacons(pos) && sensors.exists(sensorPos =>
          dist(sensorPos, pos) <= sensorRadius(sensorPos)
        )
      }

      println(result)
    } finally {
      src.close()
    }
  }

  /** @return (sensor, beacon) */
  def parseLine(line: String): ((Int, Int), (Int, Int)) = {
    val Array(x1, y1, x2, y2) = line
      .replace("Sensor at x=", "")
      .replace(": closest beacon is at x=", ",")
      .replace(" y=", "")
      .split(",")
      .map(_.toInt)
    (x1 -> y1) -> (x2 -> y2)
  }

  def dist(p1: (Int, Int), p2: (Int, Int)): Int = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    math.abs(x1 - x2) + math.abs(y1 - y2)
  }
}
