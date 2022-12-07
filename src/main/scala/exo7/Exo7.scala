package exo7

import java.nio.file.Path
import scala.collection.mutable
import scala.collection.BufferedIterator

object Exo7 {
  val fullPathToDirectory = mutable.Map.empty[Path, Directory]

  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo6/test.txt")
    try {
      val lines = src.getLines().buffered
      var cwd   = "/"

    } finally {
      src.close()
    }
  }

  /** @return (the new current directory, the computed directory) */
  def computeWorkingDirectory(
      currentDirectory: Path,
      lines: BufferedIterator[String]
  ): (Path, Directory) = {
    lines.next() match
      case "$ cd .." =>
        computeWorkingDirectory(currentDirectory.getParent, lines)
      case cmd if cmd.startsWith("$ cd ") =>
        computeWorkingDirectory(currentDirectory.resolve(cmd.stripSuffix("$ cd ")), lines)
      case "$ ls" =>
        parseLs()
  }

  def parseLs(currentDir: Path, lines: BufferedIterator[String]): Directory = {
    val (childrenDir, files) =
      lines.takeWhile(line => !line.startsWith("$")).partition(_.startsWith("dir"))

    Directory(
      name = currentDir.getFileName.toString,
      parent = Option(currentDir.getParent),
      files = files.map { line =>
        val Array(size, fileName) = line.split(" ")
        File(fileName, size.toLong)
      }.toSeq,
      children = childrenDir.map(line => currentDir.resolve(line.stripSuffix("dir "))).toSeq
    )
  }

  case class Directory(
      name: String,
      parent: Option[Path],
      files: Seq[File],
      children: Seq[Path]
  )
  case class File(name: String, size: Long)
}
