// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object Day6 {
  val SPAWN_DAYS = 7

  def readCalledOutNumbers(p: parser.Parser, ar: WrappedArray[Long])(implicit
      z: Zone
  ) = {
    val num = stackalloc[Long]

    while (string.strcmp(p.remainingString, c"\n") != 0) {
      p.long(num).const(c",")()

      ar.appendAndGrow(!num)
    }
  }

  def sum(state: Ptr[Long]) = {
    var s = 0L
    loops.loop(0, 8) { idx =>
      s += state(idx)
    }
    s
  }

  def print(state: Ptr[Long]) = {
    loops.loop(0, 8) { age =>
      stdio.printf(c"%ld ", state(age))
    }
  }

  def cycleDay(state: Ptr[Long]) = {
    val readyToSpawn = state(0)
    state(0) = state(1)
    state(1) = state(2)
    state(2) = state(3)
    state(3) = state(4)
    state(4) = state(5)
    state(5) = state(6)
    state(6) = state(7) + readyToSpawn
    state(7) = state(8)
    state(8) = readyToSpawn
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val part_1_answer = {
        val input = WrappedArray.create[Long]()
        val state = stackalloc[Long](9)
        files.lines(args.head) { line =>
          readCalledOutNumbers(parser.init(line), input)
        }

        input.foreach { fish =>
          state(fish) += 1
        }

        loops.loop(1, 80) { day =>
          cycleDay(state)
        }

        sum(state)
      }

      val part_2_answer = {
        val input = WrappedArray.create[Long]()
        val state = stackalloc[Long](9)
        files.lines(args.head) { line =>
          readCalledOutNumbers(parser.init(line), input)
        }

        input.foreach { fish =>
          state(fish) += 1
        }

        loops.loop(1, 256) { day =>
          cycleDay(state)
        }
        sum(state)
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
