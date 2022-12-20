package exo20

import java.lang.Math
import scala.collection.mutable

object Exo20 {

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo20/input.txt")
    try {
      val numbers = src.getLines().map(_.toInt).toList.zipWithIndex

      val wrapped = numbers
        .foldLeft(numbers) { case (currentList, (x, oldIndex)) =>
          val index    = currentList.indexWhere(_._2 == oldIndex)
          val newIndex = Math.floorMod(index + x, currentList.length - 1)
          val res = insertAt(
            x -> oldIndex,
            newIndex,
            currentList.patch(from = index, other = Seq(), replaced = 1)
          )
          res
        }
        .map(_._1)

      val zeroIndex = wrapped.indexWhere(_ == 0)
      val result =
        (1 to 3).map(i => wrapped(Math.floorMod(zeroIndex + i * 1000, wrapped.length))).sum

      println(result)
    } finally {
      src.close()
    }
  }

  def insertAt[A](el: A, index: Int, list: List[A]): List[A] = {
    val (upToNotIncluded, after) = list.splitAt(index)
    (upToNotIncluded :+ el) ++ after
  }
}
