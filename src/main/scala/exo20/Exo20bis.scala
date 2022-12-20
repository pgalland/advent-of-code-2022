package exo20

import java.lang.Math
import scala.collection.mutable

object Exo20bis {

  def main(args: Array[String]): Unit = {

    val src = scala.io.Source.fromFile("src/main/scala/exo20/input.txt")
    try {
      val originalNumbers =
        src.getLines().map(_.toInt).toSeq.map(x => x.toLong * 811589153).zipWithIndex
      val numbers = originalNumbers

      val decrypted = (1 to 10)
        .foldLeft(numbers) { case (currentNumbers, _) =>
          mix(originalNumbers, currentNumbers)
        }
        .map(_._1)
      val zeroIndex = decrypted.indexWhere(_ == 0)
      val result =
        (1 to 3).map(i => decrypted(Math.floorMod(zeroIndex + i * 1000, decrypted.length))).sum

      println(result)
    } finally {
      src.close()
    }
  }

  def insertAt[A](el: A, index: Int, list: Seq[A]): Seq[A] = {
    val (upToNotIncluded, after) = list.splitAt(index)
    (upToNotIncluded :+ el) ++ after
  }

  def mix(originalNumbers: Seq[(Long, Int)], numbers: Seq[(Long, Int)]): Seq[(Long, Int)] = {
    originalNumbers
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
  }
}
