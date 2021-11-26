// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc

object Day2 {

  def validate_part_1(
      from: CInt,
      to: CInt,
      char: CChar,
      str: Ptr[CChar]
  ): Boolean = {
    var i = 0
    var cnt = 0
    while (str(i) != 0) {
      if (str(i) == char) cnt += 1
      i += 1
    }

    (cnt >= from && cnt <= to)
  }

  def validate_part_2(
      first: CInt,
      second: CInt,
      char: CChar,
      str: Ptr[CChar]
  ): Boolean = {
    var i = 0
    var cnt = 0
    val firstCharIs = str(first - 1) == char
    val secondCharIs = str(second - 1) == char

    firstCharIs ^ secondCharIs
  }

  def main(args: Array[String]): Unit = {
    val MAX_LINE_LENGTH = 100
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      val from = stackalloc[CInt]
      val to = stackalloc[CInt]
      val char = stackalloc[CChar]
      val str = stackalloc[CChar](MAX_LINE_LENGTH)
      var valid_policy_1 = 0
      var valid_policy_2 = 0
      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        val parsed = stdio.sscanf(line, c"%d-%d %1c: %s\n", from, to, char, str)

        if (validate_part_1(!from, !to, !char, str)) valid_policy_1 += 1
        if (validate_part_2(!from, !to, !char, str)) valid_policy_2 += 1

      }

      println(s"Part 1: $valid_policy_1")
      println(s"Part 2: $valid_policy_2")

      stdio.fclose(file)
    }
  }
}
