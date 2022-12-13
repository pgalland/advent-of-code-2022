package exo13

import scala.collection.mutable

object Exo13bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo13/input.txt")
    try {
      val lines  = src.getLines().toSeq
      val div1   = parse("[[2]]")
      val div2   = parse("[[6]]")
      val sorted = (lines.filter(_.nonEmpty).map(parse) :+ div1 :+ div2).sortWith(order)
      val result = (sorted.indexOf(div1) + 1) * (sorted.indexOf(div2) + 1)

      println(result)
    } finally {
      src.close()
    }
  }

  trait ListOrInt

  case class MyList(elements: List[ListOrInt]) extends ListOrInt

  case class MyInt(element: Int) extends ListOrInt

  def order(left: ListOrInt, right: ListOrInt): Boolean = hasGoodOrder(left, right).get

  def hasGoodOrder(left: ListOrInt, right: ListOrInt): Option[Boolean] = {
    (left, right) match
      case (MyInt(l), MyInt(r)) => if (l == r) None else Some(l < r)
      case (MyList(l :: tl), MyList(r :: tr)) =>
        hasGoodOrder(l, r).orElse(hasGoodOrder(MyList(tl), MyList(tr)))
      case (MyList(_ :: _), MyList(Nil)) => Some(false)
      case (MyList(Nil), MyList(_ :: _)) => Some(true)
      case (MyList(Nil), MyList(Nil))    => None
      case (MyInt(l), MyList(r))         => hasGoodOrder(MyList(List(MyInt(l))), MyList(r))
      case (MyList(l), MyInt(r))         => hasGoodOrder(MyList(l), MyList(List(MyInt(r))))
  }

  def parse(s: String): ListOrInt = recParse(s).get

  def recParse(s: String): Option[ListOrInt] = {
    if (s.startsWith("[")) {
      Some(MyList(splitAtFreeCommas(s.stripPrefix("[").stripSuffix("]")).flatMap(recParse)))
    } else if (s.nonEmpty) {
      Some(MyInt(s.toInt))
    } else {
      None
    }
  }

  def splitAtFreeCommas(s: String): List[String] = {
    val (_, commaIndexes) = s.zipWithIndex
      .foldLeft(0 -> Seq.empty[Int]) { case ((level, freeCommasIndex), (c, idx)) =>
        (level, c) match
          case (0, ',') => 0           -> (freeCommasIndex :+ idx)
          case (_, ']') => (level - 1) -> freeCommasIndex
          case (_, '[') => (level + 1) -> freeCommasIndex
          case _        => level       -> freeCommasIndex
      }
    val ss = s.toCharArray
    commaIndexes.foreach(idx => ss(idx) = ';')
    ss.mkString.split(";").toList
  }
}
