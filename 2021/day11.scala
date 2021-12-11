// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.string

object Day11 {
  import Matrix._
  def readInput(file: String)(implicit z: Zone): Matrix.Typ[Int] = {
    val mp = WrappedArray.create[CString]()
    var width = 0
    files.lines(file) { line =>
      width = string.strlen(line).toInt - 1 // remove newline
      val newStr = alloc[CChar](width)
      string.strncpy(newStr, line, width.toULong)
      mp.appendAndGrow(newStr)
    }

    val storage = alloc[Int](width * mp.size)

    mp.foreach.withIndex { case (str, row) =>
      loops.loop(0, width - 1) { col =>
        storage(row * width + col) = (str(col).toInt - '0'.toInt)
      }
    }

    val struct = alloc[Matrix.Typ[Int]]
    struct._1 = storage
    struct._2 = width
    struct._3 = mp.size

    struct
  }
  def inc(row: Int, col: Int, matrix: Matrix.Typ[Int]): Int = {
    var flashed = 0
    if (matrix.valid(row, col) && matrix.at(row, col, 100) != -1) {
      val newValue = matrix.at(row, col, 100) + 1
      if (newValue > 9) {
        flashed += 1
        matrix.set(row, col, -1)
        loops.loop(-1, 1) { rowOffset =>
          loops.loop(-1, 1) { colOffset =>
            if (!(rowOffset == 0 && colOffset == 0))
              flashed += inc(row + rowOffset, col + colOffset, matrix)
          }
        }
      } else {
        matrix.set(row, col, newValue)
      }
    }

    flashed
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val part_1_answer = {
        var answer = 0
        val STEPS = 100
        val matrix = readInput(args.head)

        loops.loop(1, STEPS) { step =>
          loops.loop(0, matrix.maxRow) { row =>
            loops.loop(0, matrix.maxCol) { col =>
              answer += inc(row, col, matrix)
            }
          }
          loops.loop(0, matrix.maxRow) { row =>
            loops.loop(0, matrix.maxCol) { col =>
              if (matrix.at(row, col, 100) == -1)
                matrix.set(row, col, 0)
            }
          }
        }

        answer
      }

      val part_2_answer = {
        val matrix = readInput(args.head)
        var answer = Int.MaxValue

        loops.breakable(1, 10_000, stopWhen = answer != Int.MaxValue) { step =>
          var numFlashed = 0
          loops.loop(0, matrix.maxRow) { row =>
            loops.loop(0, matrix.maxCol) { col =>
              numFlashed += inc(row, col, matrix)
            }
          }
          loops.loop(0, matrix.maxRow) { row =>
            loops.loop(0, matrix.maxCol) { col =>
              if (matrix.at(row, col, 100) == -1)
                matrix.set(row, col, 0)
            }
          }

          if (numFlashed == 100 && step < answer)
            answer = step
        }

        answer
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
