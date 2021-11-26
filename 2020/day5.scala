// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object Day5 {

  def main(args: Array[String]): Unit = {
    def solve_part_1_better(line: CString) = {
      var num = 0
      var acc = 1
      var i = 9
      while (i >= 0) {
        if (line(i) == 'R' || line(i) == 'B')
          num += acc

        acc *= 2
        i -= 1
      }

      num
    }

    def solve_part_2(seen: Ptr[Boolean]) = {
      var i = 1
      var found = -1
      while (i < 1022 && found == -1) {
        if (!seen(i) && seen(i - 1) && seen(i + 1))
          found = i
        i += 1
      }

      found
    }

    val MAX_LINE_LENGTH = 100

    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      val seen = stackalloc[Boolean](1024) // 10 character, 2^10 = 1024

      var part1 = -1
      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        val len = string.strlen(line).toInt - 1

        val result1 = solve_part_1_better(line)
        if (result1 > part1) part1 = result1

        seen(result1) = true
      }

      println(s"Part 1: $part1")
      println(s"Part 2: ${solve_part_2(seen)}")

      stdio.fclose(file)
    }
  }
}
