// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string
import scala.scalanative.annotation.alwaysinline

object Day8 {

  object Commands {
    val NOP = c"nop"
    val JMP = c"jmp"
    val ACC = c"acc"
  }

  import string.strcmp

  def solve_part_1(
      program: WrappedArray[CStruct2[CString, Int]]
  ) = {
    val tracker = stackalloc[Boolean](program.size)
    var i = 0
    var acc = 0
    var halt = false
    while (i < program.size && !halt) {
      val cmd = program.at(i)
      halt = tracker(i)
      tracker(i) = true
      if (strcmp(cmd._1, Commands.NOP) == 0) {
        i += 1
      } else if (strcmp(cmd._1, Commands.ACC) == 0) {
        if (!halt) acc += cmd._2
        i += 1
      } else if (strcmp(cmd._1, Commands.JMP) == 0) {
        i += cmd._2
      }

    }

    acc
  }

  def terminates_normally(
      program: WrappedArray[CStruct2[CString, Int]],
      tracker: Ptr[Boolean]
  ) = {
    var i = 0
    // var acc = 0
    var halt = false
    while (i < program.size && !halt) {
      val cmd = program.at(i)
      halt = tracker(i)
      tracker(i) = true
      if (strcmp(cmd._1, Commands.NOP) == 0) {
        i += 1
      } else if (strcmp(cmd._1, Commands.ACC) == 0) {
        // if (!halt) acc += cmd._2
        i += 1
      } else if (strcmp(cmd._1, Commands.JMP) == 0) {
        i += cmd._2
      }

    }

    !halt
  }

  def solve_part_2(program: WrappedArray[CStruct2[CString, Int]]) = {

    val tracker = stackalloc[Boolean](program.size)

    def cleanup() = {
      loops.loop(0, program.size - 1) { i =>
        tracker(i) = false
      }
    }

    var found = false

    program.foreachBreakable(found) { cs =>
      if (string.strcmp(cs._1, Commands.NOP) == 0) {
        cs._1 = Commands.JMP
        if (terminates_normally(program, tracker)) {
          found = true
          println("Found it!")
        } else {
          cs._1 = Commands.NOP
        }
      } else if (string.strcmp(cs._1, Commands.JMP) == 0) {
        cs._1 = Commands.NOP
        if (terminates_normally(program, tracker)) {
          found = true
          println("Found it!")
        } else {
          cs._1 = Commands.JMP
        }
      }

      cleanup()
    }

    solve_part_1(program)
  }

  def main(args: Array[String]): Unit = {
    import string.strcmp

    val MAX_LINE_LENGTH = 100
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)

      val program = WrappedArray.create[CStruct2[CString, Int]]()
      val tmp = stackalloc[CChar](3)
      val rel = stackalloc[Int]
      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        stdio.sscanf(line, c"%s %d\n", tmp, rel)
        val cmd = stackalloc[CStruct2[CString, Int]]
        if (strcmp(tmp, Commands.NOP) == 0) cmd._1 = Commands.NOP
        else if (strcmp(tmp, Commands.ACC) == 0) cmd._1 = Commands.ACC
        else if (strcmp(tmp, Commands.JMP) == 0) cmd._1 = Commands.JMP

        cmd._2 = !rel

        program.appendAndGrow(cmd)
      }

      println(s"Part 1: ${solve_part_1(program)}")
      println(s"Part 2: ${solve_part_2(program)}")

      var foundIT = false

      stdio.fclose(file)
    }
  }
}
