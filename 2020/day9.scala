// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object Day9 {

  def main(args: Array[String]): Unit = {
    def solve_part_1() = ""
    def solve_part_2() = ""

    val MAX_LINE_LENGTH = 100
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      val ar = WrappedArray.create[Long]()
      val num = stackalloc[Long]
      var key = stackalloc[Long](25)
      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        stdio.sscanf(line, c"%ld\n", num)
        ar.appendAndGrow(!num)
      }

      var part_1_answer = -1L

      ar.foreach.breakableWithIndex(part_1_answer != -1) { case (el, idx) =>
        if (idx > 24) {
          var hasSum = false
          loops.loop(idx - 25, idx - 1) { i =>
            loops.loop(idx - 25, idx - 1) { j =>
              if (i > j) hasSum = hasSum || !ar.at(i) + !ar.at(j) == !ar.at(idx)
            }
          }
          if (!hasSum) part_1_answer = el
        }
      }

      println(s"Part 1: $part_1_answer")

      var part_2_answer_found = false
      var part_2_range_start = -1
      var part_2_range_end = -1
      ar.foreach.breakableWithIndex(part_2_answer_found) { case (el, idx) =>
        var running_sum = el
        part_2_range_start = idx
        part_2_range_end = idx
        println(s"$el")
        loops.breakable(
          idx + 1,
          ar.size - 2,
          part_2_answer_found || running_sum >= part_1_answer
        ) { i =>
          running_sum += !(ar.at(i))
          part_2_range_end = i
        }

        if (running_sum == part_1_answer) part_2_answer_found = true

      }

      if (part_2_answer_found) {
        println(s"$part_2_answer_found $part_2_range_start $part_2_range_end")
        var min_in_range = Long.MaxValue
        var max_in_range = Long.MinValue

        loops.loop(part_2_range_start, part_2_range_end - 1) { i =>
          if (!ar.at(i) < min_in_range) min_in_range = !ar.at(i)
          if (!ar.at(i) > max_in_range) max_in_range = !ar.at(i)
        }

        println(
          s"Part 2: $min_in_range + $max_in_range = ${min_in_range + max_in_range}"
        )
      }

      stdio.fclose(file)
    }
  }
}
