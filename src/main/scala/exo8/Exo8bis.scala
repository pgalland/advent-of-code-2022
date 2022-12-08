package exo8

import java.nio.file.{Path, Paths}
import scala.collection.{BufferedIterator, mutable}

object Exo8bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo8/input.txt")
    try {
      val lines   = src.getLines().toArray
      val rows    = lines.map(_.toCharArray.map(_.toString.toInt))
      val columns = rows.transpose

      val result =
        rows.indices.flatMap(i => columns.indices.map(j => (i, j))).map { case (i, j) =>
          val height = rows(i)(j)

          val left  = rows(i).slice(from = 0, until = j).reverse
          val right = rows(i).slice(from = j + 1, until = rows(i).length)
          val above = columns(j).slice(from = 0, until = i).reverse
          val below = columns(j).slice(from = i + 1, until = columns(j).length)

          val scenicScore = Seq(left, right, above, below)
            .map(direction =>
              direction
                .foldLeft((false, 0)) { case ((stop, viewingDistance), treeHeight) =>
                  if (stop) { (stop, viewingDistance) }
                  else {
                    (treeHeight >= height, viewingDistance + 1)
                  }
                }
                ._2
            )
            .product
          scenicScore
        }

      println(result.max)
    } finally {
      src.close()
    }
  }
}
