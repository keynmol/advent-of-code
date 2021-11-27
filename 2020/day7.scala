// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object Day7 {
  object parse {
    def readBagColor(input: CString, plural: Boolean)(implicit
        z: Zone
    ): CString = {
      val w1 = stackalloc[CChar](100)
      val w2 = stackalloc[CChar](100)

      if (plural) stdio.sscanf(input, c"%s %s bags", w1, w2)
      else stdio.sscanf(input, c"%s %s bag", w1, w2)

      val str = alloc[CChar](200)
      val w1_len = string.strlen(w1)

      string.strcpy(str, w1)
      str(w1_len) = ' '
      string.strcpy(str + string.strlen(w1) + 1, w2)

      str
    }
  }

  def main(args: Array[String]): Unit = {
    def solve_part_1() = ""
    def solve_part_2() = ""

    val MAX_LINE_LENGTH = 1000
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      val bagCount = stackalloc[Int]
      val rem = stackalloc[Int]

      val ar = WrappedArray.create[CStruct3[CString, CString, Int]]()
      // val counts = WrappedArray.create[CStruct2[CString, Int]]()

      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        val containerColor = parse.readBagColor(line, plural = true)
        val skip = string.strlen(containerColor).toInt + " bags contain ".length
        val noOtherBags = string.strncmp(line + skip, c"no", 2.toUInt) == 0

        if (!noOtherBags) {
          val newLine = line + skip
          val originalLength = string.strlen(newLine).toInt
          var remaining = originalLength
          // stdio.printf(c">'%s'\n", line)
          // stdio.printf(c">>'%s'\n", newLine)
          while (remaining > 2) {
            val next = newLine + (originalLength - remaining)
            // stdio.printf(c">>>'%s'\n", next)
            stdio.sscanf(next, c"%d%n", bagCount, rem)

            val isSeveral = !bagCount == 1

            val bagColor =
              if (!isSeveral)
                parse.readBagColor(next + 2, plural = false)
              else parse.readBagColor(next + !rem + 1, plural = true)

            val toSkip = {
              string.strlen(bagColor).toInt + 1 + // space
                { if (isSeveral) 4 else 3 } + 1 + !rem + 1
            }

            if (next(toSkip) == ',') {
              remaining = remaining - toSkip - 2
            } else remaining = remaining - toSkip
            // stdio.printf(c"%d, %s, %d\n", !bagCount, bagColor, remaining)

            val mem = stackalloc[CStruct3[CString, CString, Int]]
            mem._1 = bagColor
            mem._2 = containerColor
            mem._3 = !bagCount

            ar.appendAndGrow(!mem)

          }
        }

      }

      val enclosure = WrappedArray.create[CString]()

      def mark(color: CString): Unit = {
        var found = false
        enclosure.foreach.breakable(found) { cs =>
          if (string.strcmp(cs, color) == 0) found = true
        }

        if (found) {
          // stdio.printf(c"Already marked: %s\n", color)
        } else {
          enclosure.appendAndGrow(color)
          ar.foreach { ptr =>
            if (string.strcmp(ptr._1, color) == 0)
              mark(ptr._2)

          }
        }

      }

      def sum(color: CString): Int = {
        var cnt = 0

        ar.foreach { cs =>
          if (string.strcmp(cs._2, color) == 0) {
            val containerBag = cs._2
            val number = cs._3
            val insideBag = cs._1
            cnt += number + number * sum(insideBag)
          }
        }

        cnt
      }

      // ar.foreach { cs =>
      //   stdio.printf(c"%s -> %s\n", cs._1, cs._2)
      // }

      mark(c"shiny gold")

      // enclosure.foreach { cs =>
      //   stdio.printf(c"Can contain shiny gold: %s\n", cs)
      // }

      val part_1_answer = enclosure.size - 1
      var part_2_answer = sum(c"shiny gold")
      println(s"Part 1: $part_1_answer")
      println(s"Part 2: $part_2_answer")

      stdio.fclose(file)
    }
  }
}
