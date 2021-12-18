// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.libc.string
import scala.scalanative.libc.stdlib

object Day9 {
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

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val part_1_answer = {
        var answer = 0
        val inp = readInput(args.head)

        loops.loop(0, inp.maxRow) { row =>
          loops.loop(0, inp.maxCol) { col =>
            val center = inp.at(row, col, 10)
            val isLowPoint =
              center < inp.left(row, col, 10) &&
                center < inp.right(row, col, 10) &&
                center < inp.down(row, col, 10) &&
                center < inp.up(row, col, 10)

            if (isLowPoint)
              answer += 1 + center

          }
        }
        answer
      }

      val part_2_answer = {
        var answer = 1
        val inp = readInput(args.head)

        def pack(row: Int, col: Int) = {
          (row << 16) + col
        }

        def contains(visited: WrappedArray[Int], packed: Int) = {
          var found = false
          loops.breakable(0, visited.size - 1, stopWhen = found) { idx =>
            found = !visited.at(idx) == packed
          }
          found
        }

        def growBasin(row: Int, col: Int, visited: WrappedArray[Int]): Unit = {
          val packed = pack(row, col)
          val cur = inp.at(row, col, 100)
          if (cur != 100 && cur != 9) {
            if (!contains(visited, packed)) {
              visited.appendAndGrow(packed)
              val left = inp.left(row, col, 100)
              val right = inp.right(row, col, 100)
              val down = inp.down(row, col, 100)
              val up = inp.up(row, col, 100)

              if (down != 100 && down > cur) growBasin(row + 1, col, visited)
              if (up != 100 && up > cur) growBasin(row - 1, col, visited)
              if (left != 100 && left > cur) growBasin(row, col - 1, visited)
              if (right != 100 && right > cur) growBasin(row, col + 1, visited)
            }
          }
        }

        val basins = WrappedArray.create[Int]()

        loops.loop(0, inp.maxRow) { row =>
          loops.loop(0, inp.maxCol) { col =>
            val center = inp.at(row, col, 10)
            val isLowPoint =
              center < inp.left(row, col, 10) &&
                center < inp.right(row, col, 10) &&
                center < inp.down(row, col, 10) &&
                center < inp.up(row, col, 10)

            if (isLowPoint) {
              val visited = WrappedArray.create[Int]()
              growBasin(row, col, visited)
              basins.appendAndGrow(visited.size)
            }

          }
        }

        val continuous = stackalloc[Int](basins.size)

        basins.continuous(continuous)

        stdlib.qsort(
          continuous.asInstanceOf[Ptr[Byte]],
          basins.size.toULong,
          sizeof[Int],
          CFuncPtr2.fromScalaFunction { case (a1, a2) =>
            -1 * (!(a1.asInstanceOf[Ptr[Int]]) - !(a2.asInstanceOf[Ptr[Int]]))
          }
        )
        answer *= continuous(0)
        answer *= continuous(1)
        answer *= continuous(2)

        answer
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
