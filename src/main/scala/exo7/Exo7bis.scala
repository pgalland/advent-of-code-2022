package exo7

import java.nio.file.{Path, Paths}
import scala.collection.{BufferedIterator, mutable}

object Exo7bis {
  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo7/input.txt")
    try {
      val lines = src.getLines()
      val commandsAndLsResultPairs = lines
        .foldLeft(Seq.empty[mutable.Buffer[String]]) { case (acc, line) =>
          if (acc.isEmpty) {
            Seq(mutable.Buffer(line))
          } else if (acc.last.head.isCmd == line.isCmd) {
            acc.last.append(line)
            acc
          } else {
            acc :+ mutable.Buffer(line)
          }
        }
        .map(_.toSeq)
        .sliding(2, 2) // each group is "sequence of cd then ls" -> result of ls

      val (_, partialDirectories) =
        commandsAndLsResultPairs.foldLeft(Paths.get("/") -> Seq.empty[DirectoryPartial]) {
          case ((cwd, accDirs), Seq(commands, lsResult)) =>
            val newCwd = commands
              .dropRight(1) // drop the ls at the end.
              .foldLeft(cwd) { case (workingDir, cmd) =>
                val dir = cmd.stripPrefix("$ cd ")
                if (dir == "..") workingDir.getParent else workingDir.resolve(dir)
              }
            val (childrenDir, files) = lsResult.partition(_.startsWith("dir"))
            newCwd -> (
              accDirs :+ DirectoryPartial(
                path = newCwd,
                files = files.map { line =>
                  val Array(size, name) = line.split(" ")
                  File(name, size.toLong)
                },
                children = childrenDir.map(line => newCwd.resolve(line.stripPrefix("dir ")))
              )
            )
        }

      val pathToDirectoryPartial = partialDirectories.map(dir => dir.path -> dir).toMap
      val directories            = partialDirectories.map(_.toDirectory(pathToDirectoryPartial))
      val totalSize              = directories.find(_.path.toString == "/").get.size
      val freeSpace              = 70000000 - totalSize
      val sizeNeeded             = 30000000 - freeSpace
      val result                 = directories.map(_.size).filter(size => size >= sizeNeeded).min
      println(result)
    } finally {
      src.close()
    }
  }

  case class DirectoryPartial(
      path: Path,
      files: Seq[File],
      children: Seq[Path]
  ) {
    def toDirectory(pathToDirectoryPartial: Map[Path, DirectoryPartial]): Directory = {
      Directory(
        path,
        files,
        children.map(pathToDirectoryPartial).map(_.toDirectory(pathToDirectoryPartial))
      )
    }
  }

  case class Directory(
      path: Path,
      files: Seq[File],
      children: Seq[Directory]
  ) {
    def size: Long = {
      files.map(_.size).sum + children.map(child => child.size).sum
    }
  }

  case class File(name: String, size: Long)

  extension (line: String) def isCmd: Boolean = line.startsWith("$")
}
