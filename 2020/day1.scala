// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio

object Day1 {
  def solve_part_1(array: WrappedArray[Int]) = {
    var found = -1

    array.foreach.breakable(found != -1) { el1 =>
      array.foreach.breakable(found != -1) { el2 =>
        if (el1 + el2 == 2020) {
          println(s"> $el1, $el2")
          found = el1 * el2
        }
      }
    }

    found
  }

  def solve_part_2(array: WrappedArray[Int]) = {
    var found = -1

    array.foreach.breakable(found != -1) { el1 =>
      array.foreach.breakable(found != -1) { el2 =>
        array.foreach.breakable(found != -1) { el3 =>
          if (el1 + el2 + el3 == 2020) {
            println(s"> $el1, $el2, $el3")
            found = el1 * el2 * el3
          }
        }
      }
    }

    found
  }

  def main(args: Array[String]): Unit = {
    val MAX_LINE_LENGTH = 10
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      val num = stackalloc[CInt]
      val arr = WrappedArray.create[CInt]()
      var i = 0
      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        stdio.sscanf(line, c"%d\n", num)

        arr.appendAndGrow(!num)

        i = i + 1;
      }

      println(s"Part 1:" + solve_part_1(arr).toString)
      println(s"Part 2:" + solve_part_2(arr).toString)

      stdio.fclose(file)
    }
  }
}
