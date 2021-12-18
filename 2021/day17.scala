// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.annotation.alwaysinline
import scala.scalanative.unsafe._

import scalanative.libc.stdio
import scala.scalanative.libc

object Day17 {

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val num = stackalloc[Int]

      val part_1_answer = {
        var answer = 0
        val ranges = alloc[CStruct2[Int, Int]](2)
        val xrange = !ranges
        val yrange = !(ranges + 1)

        @alwaysinline def inRange(x: Int, y: Int) = {
          x >= xrange._1 && x <= xrange._2 && y >= yrange._1 && y <= yrange._2
        }

        def visualise(
            velocityX: Int,
            velocityY: Int,
            debug: Boolean = false
        ) = {
          var inside = -1

          var x = 0
          var y = 0
          var velX = velocityX
          var velY = velocityY

          var maxY = Int.MinValue
          var earlyStop = false

          loops.breakable(
            0,
            1000,
            stopWhen = inside != -1 && !earlyStop
          ) { step =>
            if (debug)
              stdio.printf(
                c"x=%d, y = %d, velX = %d, velY = %d\n",
                x,
                y,
                velX,
                velY
              )
            x += velX
            y += velY
            if (y > maxY) maxY = y
            velY -= 1
            velX += { if (velX < 0) 1 else if (velX > 0) -1 else 0 }

            earlyStop = earlyStop || (velY < 0 && y < yrange._1)
            earlyStop = earlyStop || (velX >= 0 && x > xrange._2)

            if (inRange(x, y)) inside = step
          }

          if (inside != -1) maxY else Int.MinValue
        }
        var maxHeight = Int.MinValue

        files.parsedLines(args.head) { case (_, parser) =>
          parser
            .const(c"target area: x=")()
            .int((ranges + 0).at1)
            .const(c"..")()
            .int((ranges + 0).at2)
            .const(c", y=")()
            .int((ranges + 1).at1)
            .const(c"..")()
            .int((ranges + 1).at2)
          
          // here be math dragons
          val minVx =
            math.sqrt(
              2 * xrange._1 + 0.25
            ) - 1 // actual value is 0.5, but just for safety
          val maxVx = xrange._2


          // if you don't publicise your solution, no one
          // will judge you
          val minVy = -100
          val maxVy = 100

          var part_2 = 0

          loops.loop(minVx.toInt, maxVx) { vx =>
            loops.loop(minVy, maxVy) { vy =>
              if (
                !(yrange._1 <= 0 && vy < yrange._1) && !(yrange._1 > 0 && vy > yrange._2)
              ) {
                val result = visualise(vx, vy)
                if (result > maxHeight) {
                  maxHeight = result
                }
                if (result != Int.MinValue) part_2 += 1
              }
            }
          }

          println(s"Part 1: $maxHeight")
          println(s"Part 2: $part_2")
        }
      }

    }
  }
}
