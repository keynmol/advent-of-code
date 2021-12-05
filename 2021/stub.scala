// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object DayStub {

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val num = stackalloc[Int]

      val part_1_answer = {
        var answer = 0
        files.lines(args.head) { line =>
          stdio.sscanf(line, c"%d\n", num)
        }
        answer
      }

      val part_2_answer = {
        var answer = 0
        files.linesWithIndex(args.head) { case (line, idx) =>
          stdio.sscanf(line, c"%d\n", num)
        }

        answer
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
