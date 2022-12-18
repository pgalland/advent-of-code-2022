package exo17

import scala.collection.mutable

object Exo17 {
  import Shape._
  import Direction._

  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo17/input.txt")
    try {
      val flow   = parseLine(src.getLines().next())
      val points = mutable.Set.empty[(Int, Int)]

      def maxY: Int    = points.map(_._2).maxOption.getOrElse(0)
      var flowIdx: Int = 0

      (0 until 2022).foreach { nb =>
        val shape                       = Shape.fromOrdinal(nb % 5)
        var struct                      = Struct(shape, maxY)
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
      }

      println(maxY)
    } finally {
      src.close()
    }
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
