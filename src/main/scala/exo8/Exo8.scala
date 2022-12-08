package exo8

import java.nio.file.{Path, Paths}
import scala.collection.mutable
import scala.collection.BufferedIterator
object Exo8 {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo8/input.txt")
    try {
      val lines   = src.getLines().toArray
      val rows    = lines.map(_.toCharArray.map(_.toString.toInt))
      val columns = rows.transpose

      val result =
        rows.indices.flatMap(i => columns.indices.map(j => (i, j))).count { case (i, j) =>
          val height = rows(i)(j)

          val left  = rows(i).slice(from = 0, until = j)
          val right = rows(i).slice(from = j + 1, until = rows(i).length)
          val above = columns(j).slice(from = 0, until = i)
          val below = columns(j).slice(from = i + 1, until = columns(j).length)

          Seq(left, right, above, below).exists(direction =>
            direction.forall(treeHeight => height > treeHeight)
          )
        }

      println(result)
    } finally {
      src.close()
    }
  }
}
