// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string
import scala.scalanative.annotation.alwaysinline

object DayStub {
  @alwaysinline def max(a: Int, b: Int) = if(a > b) a else b
  @alwaysinline def min(a: Int, b: Int) = if(a > b) b else a

  type Line = CStruct4[Int, Int, Int, Int]
  type Size = CStruct2[Int, Int]

  def isDiagonal(l: Line) = {
    val yDiff = l._2 - l._4
    val xDiff = l._1 - l._3

    yDiff == xDiff || yDiff == -xDiff
  }


  def readLines(file: String, lines: WrappedArray[Line], diagonal: Boolean)(implicit z: Zone) = {
      val x1 = stackalloc[Int]
      val x2 = stackalloc[Int]
      val y1 = stackalloc[Int]
      val y2 = stackalloc[Int]

      var answer = 0
      var maxX = -1
      var maxY = -1
      files.parsedLines(file) { case (line, parser) =>
        parser.int(x1).const(c",")().int(y1).const(c" -> ")().int(x2).const(c",")().int(y2)
          val st = stackalloc[CStruct4[Int, Int, Int, Int]]
          st._1 = !x1
          st._2 = !y1
          st._3 = !x2
          st._4 = !y2
        if(!x1 == !x2 || !y1 == !y2 || (diagonal && isDiagonal(st))) {
          if(!x1 > maxX) maxX = !x1
          if(!x2 > maxX) maxX = !x2
          if(!y1 > maxY) maxY = !y1
          if(!y2 > maxY) maxY = !y2
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
        var answer = 0
        val lines = WrappedArray.create[CStruct4[Int, Int, Int, Int]]()
        val size = readLines(args.head, lines, diagonal = false)
        val width = size._1
        val height = size._2

        val MAP = stackalloc[Int](width * height)


        lines.foreach {line => 
          loops.loop(min(line._1, line._3), max(line._1, line._3)) {col => 
            loops.loop(min(line._2, line._4), max(line._2, line._4)) {row => 
                MAP(row * width + col) += 1
            }
          }
        }

        loops.loop(0, width*height, inclusive = false) {n => 
          if(MAP(n) > 1) answer +=1
        }


        answer
      }

      val part_2_answer = {
        var answer = 0
        val lines = WrappedArray.create[CStruct4[Int, Int, Int, Int]]()
        val size = readLines(args.head, lines, diagonal = true)
        val width = size._1
        val height = size._2

        val MAP = stackalloc[Int](width * height)


        lines.foreach {line => 
          if(!isDiagonal(line)) {
            loops.loop(min(line._1, line._3), max(line._1, line._3)) {col => 
              loops.loop(min(line._2, line._4), max(line._2, line._4)) {row => 
                  MAP(row * width + col) += 1
              }
            }
          }
        }

        println("-------------")

        lines.foreach { line => 
          if(isDiagonal(line)) {
            val yIncrement = if(line._2 > line._4) -1 else +1
            val xIncrement = if(line._1 > line._3) -1 else +1
            var col = line._1
            var row = line._2

            while(!(col == line._3 && row == line._4)) {
              MAP(row * width + col) += 1
              col += xIncrement
              row += yIncrement
            }
            MAP(row * width + col) += 1
          }
        }


        loops.loop(0, width*height, inclusive = false) {n => 
          if(MAP(n) > 1) answer +=1
        }


        answer
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
