package exo10

import exo10.Exo10bis.State.{FetchInstruction, Wait}

import java.nio.file.{Path, Paths}
import scala.collection.{BufferedIterator, mutable}
import scala.util.Try

object Exo10bis {
  val signalCycles = Set(20, 60, 100, 140, 180, 220)
  // registers
  var tempX: Int                 = 0
  var X: Int                     = 1
  var instr: Option[Instruction] = Some(Noop)

  def main(args: Array[String]): Unit = {
    val src = scala.io.Source.fromFile("src/main/scala/exo10/input.txt")
    try {
      val lines = src.getLines()

      var signalStrength = 0L

      val instructionStack = mutable.Stack.from(lines.map(parseInstruction))
      var clock: Int       = 0
      var state: State     = FetchInstruction
      var break: Boolean   = false

      val screen = (1 to 6).map(_ => Array.fill(40)("."))

      while (!break) {
        // enter the new state when clock -> clock+1
        latch(state, instructionStack)
        clock += 1
        // logic in the new state
        // draw on screen
        val (row, column) = clockToCoordinates(clock)
        if (column == X - 1 || column == X || column == X + 1) {
          screen(row)(column) = "#"
        }

        // continue execution
        (state, instr) match
          case (_, None)                      => break = true
          case (Wait, _)                      => state = FetchInstruction
          case (FetchInstruction, Some(Noop)) => state = FetchInstruction
          case (FetchInstruction, Some(Add(x))) =>
            state = Wait
            tempX = x
      }

      screen.foreach { row =>
        row.foreach(print)
        println()
      }
    } finally {
      src.close()
    }
  }

  def latch(nextState: State, instructionStack: mutable.Stack[Instruction]) = nextState match
    case Wait => ()
    case FetchInstruction =>
      instr = Try(instructionStack.pop()).toOption
      X = X + tempX
      tempX = 0

  sealed trait Instruction
  case object Noop       extends Instruction
  case class Add(X: Int) extends Instruction

  enum State:
    case FetchInstruction, Wait

  def parseInstruction(line: String): Instruction = line match
    case "noop" => Noop
    case _      => Add(line.split(" ").last.toInt)

  def clockToCoordinates(clock: Int): (Int, Int) = {
    val row    = (clock - 1) / 40
    val column = (clock - 1) % 40
    row -> column
  }
}
