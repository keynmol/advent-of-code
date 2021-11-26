// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object DayStub {

  def main(args: Array[String]): Unit = {
    def solve_part_1 = ""
    def solve_part_2 = ""

    val MAX_LINE_LENGTH = 100
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {}

      val world = World(trees, patternWidth, patternHeight)

      println(s"Part 1: ${solve_part_1()}")
      println(s"Part 2: ${solve_part_2()}")

      stdio.fclose(file)
    }
  }
}
