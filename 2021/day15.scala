// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.libc.string
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.libc.stdio
import scala.scalanative.runtime.libc
import scala.scalanative.libc.stdlib

object Day15 {
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
    import Matrix._
    Zone.apply { implicit z =>
      def dijkstra(labyrinth: Matrix.Typ[Int]) = {
        import Matrix._
        import Bitset._
        val INFINITY = Int.MaxValue

        val Q = Bitset.create(labyrinth.width * labyrinth.height)
        loops.loop(0, labyrinth.width * labyrinth.height - 1) { idx =>
          Q.set(idx + 1)
        }

        val dist = Matrix.create[Int](labyrinth.width, labyrinth.height)
        dist.reset(INFINITY)
        dist.set(labyrinth.maxRow, labyrinth.maxCol, 0)

        val prev = Matrix.create[Int](labyrinth.width, labyrinth.height)
        prev.reset(-1)

        loops.loop(0, labyrinth.maxRow) { row =>
          loops.loop(0, labyrinth.maxCol) { col =>
            val weight = labyrinth.unsafe(row, col)
            loops.loop(-1, 1) { rowOffset =>
              loops.loop(-1, 1) { colOffset =>
                import scala.scalanative.libc.math.abs

                if (abs(rowOffset) + abs(colOffset) == 1) {
                  val vRow = row + rowOffset
                  val vCol = col + colOffset

                  if (labyrinth.at(vRow, vCol, -1) != -1) {
                    val from = dist.pack(vRow, vCol)
                    val to = dist.pack(row, col)

                    stdio.printf(c"%d %d %d\n", from, to, weight)
                  }
                }
              }
            }
          }
        }

        def getMin: Int = {
          var minDistance = Int.MaxValue
          var found = -1
          loops.loop(0, dist.width * dist.height - 1) { idx =>
            val row = dist.unpackRow(idx)
            val col = dist.unpackCol(idx)

            val inSet = Q.get(idx + 1)
            if (inSet && dist.unsafe(row, col) < minDistance) {
              found = dist.pack(row, col)
              minDistance = dist.unsafe(row, col)
            }
          }

          found
        }

        var i = 0f

        while (!Q.empty) {
          val next = getMin
          val uRow = dist.unpackRow(next)
          val uCol = dist.unpackCol(next)
          Q.unset(next + 1)
          i += 1

          if (i % 100.0f == 0)
            stdio.printf(
              c"Processed %.2f\n",
              100 * i / (dist.width * dist.height)
            )

          loops.loop(-1, 1) { rowOffset =>
            loops.loop(-1, 1) { colOffset =>
              import scala.scalanative.libc.math.abs

              if (abs(rowOffset) + abs(colOffset) == 1) {
                val vRow = uRow + rowOffset
                val vCol = uCol + colOffset
                val vIdx = dist.pack(vRow, vCol) + 1
                val length = labyrinth.at(uRow, uCol, -1)
                val dist_v = dist.at(vRow, vCol, -1)
                // stdio.printf(
                //   c"For cell %d:%d visiting its neighbour %d:%d (offset: %d:%d) (%d)\n",
                //   uRow,
                //   uCol,
                //   vRow,
                //   vCol,
                //   rowOffset,
                //   colOffset,
                //   length
                // )

                if (dist_v != -1 && Q.get(vIdx)) {
                  val dist_u = dist.unsafe(uRow, uCol)
                  val alt = dist_u + length
                  // stdio.printf(
                  //   c"Distance: %d, alternative: %d + %d = %d\n",
                  //   dist_v,
                  //   dist_u,
                  //   length,
                  //   alt
                  // )

                  if (alt < dist_v) {
                    // stdio.printf(c"Setting %d:%d to %d\n", vRow, vCol, alt)
                    prev.set(vRow, vCol, dist.pack(uRow, uCol))
                    dist.set(vRow, vCol, alt)
                  }
                }
              }
            }
          }
        }
        (dist, prev)
      }

      val part_1_answer = {
        val matrix = readInput(args.head)
        val (results, prev) = dijkstra(matrix)

        val red = toCString(Console.RED)
        val yello = toCString(Console.YELLOW)
        val green = toCString(Console.CYAN + Console.BOLD)
        val reset = toCString(Console.RESET)

        val optimal = Bitset.create(results.width * results.height)
        import Bitset._

        def findOptimal = {
          var u = results.pack(0, 0)
          val UNDEFINED = -1

          while (u != UNDEFINED) {
            optimal.set(u)
            u = prev.unsafe(
              prev.unpackRow(u),
              prev.unpackCol(u)
            )
          }
        }

        findOptimal
        stdio.printf(yello)
        stdio.printf(c"    ")
        loops.loop(0, matrix.maxCol) { col =>
          stdio.printf(c"%6d", col)
        }
        stdio.printf(reset)
        stdio.printf(c"\n")

        loops.loop(0, matrix.maxRow) { row =>
          stdio.printf(yello)
          stdio.printf(c"%4d", row)
          stdio.printf(reset)
          loops.loop(0, matrix.maxCol) { col =>
            val elId = row * matrix.width + col
            stdio.printf(c"%4d", results.unsafe(row, col))
            if (optimal.get(elId))
              stdio.printf(green)
            else
              stdio.printf(red)
            stdio.printf(c"|%1d", matrix.unsafe(row, col))
            stdio.printf(reset)

          }
          stdio.printf(c"\n")
        }

        results.unsafe(0, 0)
      }

      val part_2_answer = {}

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
