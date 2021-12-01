// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object Day1 {

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val num = stackalloc[Int]

      val part_1_answer = {
        var prev = -1
        var answer = 0
        files.parsedLines(args.head) { case (line, parser) =>
          parser.int(num).newline()
          val n2 = (!num)
          if (n2 > prev) { if (prev != -1) answer += 1; }
          prev = n2
        }
        answer
      }

      val part_2_answer = {
        var `n-1` = -1
        var `n-2` = -1
        var prev_sum = -1
        var answer = 0
        files.parsedLinesWithIndex(args.head) { case (line, parser, idx) =>
          parser.int(num).newline()
          if (idx >= 2) {
            val current = !num + `n-2` + `n-1`
            if (current > prev_sum && prev_sum != -1) answer += 1
            prev_sum = current
          }
          `n-2` = `n-1`
          `n-1` = !num

        }

        answer
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
