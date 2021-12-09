// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.annotation.alwaysinline

object Day5 {
  @alwaysinline def max(a: Int, b: Int) = if (a > b) a else b
  @alwaysinline def min(a: Int, b: Int) = if (a > b) b else a

  type Line = CStruct4[Int, Int, Int, Int]
  type Size = CStruct2[Int, Int]

  def isDiagonal(l: Line) = {
    val yDiff = l._2 - l._4
    val xDiff = l._1 - l._3

    yDiff == xDiff || yDiff == -xDiff
  }
  
  def isHorizontal(l: Line) = {
    val yDiff = l._2 - l._4
    val xDiff = l._1 - l._3

    yDiff == 0 || xDiff == 0
  }

  def readLines(file: String, lines: WrappedArray[Line], diagonal: Boolean)(
      implicit z: Zone
  ) = {
    var maxX = -1
    var maxY = -1
    files.parsedLines(file) { case (_, parser) =>
      val st = stackalloc[CStruct4[Int, Int, Int, Int]]
      parser
        .int(st.at1)
        .const(c",")()
        .int(st.at2)
        .const(c" -> ")()
        .int(st.at3)
        .const(c",")()
        .int(st.at4)
      if (isHorizontal(st) || (diagonal && isDiagonal(st))) {
        if (st._1 > maxX) maxX = st._1
        if (st._3 > maxX) maxX = st._3
        if (st._2 > maxY) maxY = st._2
        if (st._4 > maxY) maxY = st._4
        lines.appendAndGrow(st)
      }
    }

    val size = alloc[CStruct2[Int, Int]]
    size._1 = maxX + 1
    size._2 = maxY + 1

    size
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val part_1_answer = {
        val lines = WrappedArray.create[CStruct4[Int, Int, Int, Int]]()
        val size = readLines(args.head, lines, diagonal = false)
        val width = size._1
        val height = size._2

        val MAP = stackalloc[Int](width * height)

        lines.foreach { line =>
          val yIncrement =
            if (line._2 > line._4) -1 else if (line._2 == line._4) 0 else +1
          val xIncrement =
            if (line._1 > line._3) -1 else if (line._1 == line._3) 0 else +1
          var col = line._1
          var row = line._2

          while ({
            MAP(row * width + col) += 1
            !(col == line._3 && row == line._4)
          }) {
            col += xIncrement
            row += yIncrement
          }
        }

        var answer = 0
        loops.loop(0, width * height, inclusive = false) { n =>
          if (MAP(n) > 1) answer += 1
        }

        answer
      }

      val part_2_answer = {
        val lines = WrappedArray.create[CStruct4[Int, Int, Int, Int]]()
        val size = readLines(args.head, lines, diagonal = true)
        val width = size._1
        val height = size._2

        val MAP = stackalloc[Int](width * height)

        lines.foreach { line =>
          val yIncrement =
            if (line._2 > line._4) -1 else if (line._2 == line._4) 0 else +1
          val xIncrement =
            if (line._1 > line._3) -1 else if (line._1 == line._3) 0 else +1
          var col = line._1
          var row = line._2

          while ({
            MAP(row * width + col) += 1
            !(col == line._3 && row == line._4)
          }) {
            col += xIncrement
            row += yIncrement
          }
        }

        var answer = 0
        loops.loop(0, width * height, inclusive = false) { n =>
          if (MAP(n) > 1) answer += 1
        }

        answer
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
