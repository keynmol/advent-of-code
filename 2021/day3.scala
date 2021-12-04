// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string
import scala.scalanative.annotation.alwaysinline

object Day3 {

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val SIZE = if(args.head.endsWith("-sample")) 5 else 12
      val frequencies = stackalloc[Int](SIZE)
      var total = 0

      def getMeThatNumba(str: CString) = {
        var numba = 0
        loops.loop(0, SIZE - 1) { idx =>
          if (str(idx) == '1') {
            numba = (numba << 1) + 1
          } else {
            numba = (numba << 1)
          }
        }

        numba
      }

      val part_1_answer = {
        files.lines(args.head) { line =>
          loops.loop(0, SIZE - 1) { idx =>
            if (line(idx) == '1') frequencies(idx) += 1
          }
          total += 1
        }
        var gamma = 0
        var epsilon = 0
        loops.loop(0, SIZE - 1) { idx =>
          if (frequencies(idx) > total / 2) {
            gamma = (gamma << 1) + 1
            epsilon = epsilon << 1
          } else {
            gamma = (gamma << 1)
            epsilon = (epsilon << 1) + 1
          }
        }

        gamma * epsilon
      }

      val part_2_answer = {
        val ar = WrappedArray.create[Int]()

        files.lines(args.head) { l => ar.appendAndGrow(getMeThatNumba(l)) }

        var o2Mask = 0
        var co2Mask = 0

        var oxygenRating = -1
        var co2Rating = -1

        loops.loop(1, SIZE + 1) { bit =>
          val bitMask = 1 << (SIZE - bit)

          var o2Processed = 0
          var o2Ones = 0
          var co2Processed = 0
          var co2Ones = 0

          ar.foreach { num =>
            val hasOne = if ((num & bitMask) != 0) 1 else 0

            val shift = SIZE - bit + 1
            val goodO2 = (num >>> shift) == (o2Mask >>> shift)
            val goodCO2 = (num >>> shift) == (co2Mask >>> shift)

            if (goodO2) {
              o2Processed += 1
              o2Ones += hasOne
              oxygenRating = num
            }

            if (goodCO2) {
              co2Processed += 1
              co2Ones += hasOne
              co2Rating = num
            }
          }
          val o2Zeroes = o2Processed - o2Ones
          val co2Zeroes = co2Processed - co2Ones

          if (o2Ones >= o2Zeroes && o2Processed > 1)
            o2Mask += bitMask // 1 is most common
          if (co2Zeroes > co2Ones && co2Processed > 1)
            co2Mask += bitMask // 1 is least common

        }

        oxygenRating * co2Rating
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
