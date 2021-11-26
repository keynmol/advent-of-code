// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object Day6 {

  def main(args: Array[String]): Unit = {
    def solve_part_1() = ""
    def solve_part_2() = ""

    val MAX_LINE_LENGTH = 100
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      val buf = stackalloc[Int](26)
      var part_1_count = 0

      var part_2_count = 0
      var group_size = 0

      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        if (string.strlen(line).toInt == 1) {
          loops.loop(0, 25) { i =>
            if (buf(i) != 0) {
              part_1_count += 1
              if (buf(i) == group_size) part_2_count += 1
            }
            buf(i) = 0
          }
          group_size = 0
        } else {
          group_size += 1
          strings.foreachChar(line) { char =>
            if (char != '\n') {
              buf(char.toInt - 97) = buf(char.toInt - 97) + 1
            }
          }
        }
      }

      println(s"Part 1: ${part_1_count}")
      println(s"Part 2: ${part_2_count}")

      stdio.fclose(file)
    }
  }
}
