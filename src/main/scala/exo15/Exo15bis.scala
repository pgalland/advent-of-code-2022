package exo15

import scala.collection.mutable
import math.{min, max, abs, signum}

object Exo15bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo15/input.txt")
    try {
      val lines            = src.getLines().toSeq
      val sensorsToBeacons = lines.map(parseLine).toMap
      val sensorRadius     = sensorsToBeacons.map { case (s, b) => s -> dist(s, b) }

      val slicedDisks = sensorRadius.map { case (center, radius) => sliceDisk(center, radius) }

      val limit = 4000000

      val (xx, yy) = (0 to limit).flatMap { y =>
        val slices = slicedDisks.flatMap(slicedDisk => slicedDisk.get(y)).toSeq.sorted

        val (_, hole) = slices.foldLeft[(Int, Option[(Int, Int)])](-1 -> None) {
          case ((pointer, None), (xStart, xEnd)) =>
            if (pointer + 1 < xStart) {
              0 -> Some((pointer + 1) -> y)
            } else {
              max(pointer, xEnd) -> None
            }
          case ((_, Some(result)), _) => 0 -> Some(result)
        }
        hole
      }.head

      println(4000000 * xx.toLong + yy.toLong)
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
    abs(x1 - x2) + abs(y1 - y2)
  }

  /** @return the slice (xStart, xEnd) for y = ? */
  def sliceDisk(center: (Int, Int), radius: Int): Map[Int, (Int, Int)] = {
    val (xc, yc) = center
    (yc - radius to yc + radius).map { y =>
      val xStart = xc - (radius - abs(y - yc))
      val xEnd   = xc + (radius - abs(y - yc))
      y -> (xStart, xEnd)
    }.toMap
  }
}
