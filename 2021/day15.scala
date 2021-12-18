// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.libc.stdio
import scala.scalanative.libc.string
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

object Day15 {
  private def readInput(file: String)(implicit z: Zone): Matrix.Typ[Int] = {
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

  private def printGraph(labyrinth: Matrix.Typ[Int]) = {
    import Matrix._
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
                val from = labyrinth.pack(vRow, vCol)
                val to = labyrinth.pack(row, col)

                stdio.printf(c"%d %d %d\n", from, to, weight)
              }
            }
          }
        }
      }
    }
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

        def getMin: Int = {
          var minDistance = Int.MaxValue
          var found = -1
          Q.foreach { idx =>
            val row = dist.unpackRow(idx - 1)
            val col = dist.unpackCol(idx - 1)

            val candidate = dist.unsafe(row, col)
            if (candidate < minDistance) {
              found = idx - 1
              minDistance = candidate
            }
          }

          found
        }

        var i = 0
        val size = dist.width * dist.height
        val start = System.currentTimeMillis()
        var stop = false

        while (!Q.empty && !stop) {
          val next = getMin
          if (next == 0) {
            stop = true
          } else {
            val uRow = dist.unpackRow(next)
            val uCol = dist.unpackCol(next)

            Q.unset(next + 1)
            i += 1

            var rowOffset = -1

            while (rowOffset <= 1) {
              var colOffset = -1
              while (colOffset <= 1) {
                import scala.scalanative.libc.math.abs

                if (abs(rowOffset) + abs(colOffset) == 1) {
                  val vRow = uRow + rowOffset
                  val vCol = uCol + colOffset
                  val vIdx = dist.pack(vRow, vCol) + 1

                  if (dist.valid(vRow, vCol) && Q.get(vIdx)) {
                    val dist_v = dist.at(vRow, vCol, -1)
                    val length = labyrinth.at(uRow, uCol, -1)
                    val dist_u = dist.unsafe(uRow, uCol)
                    val alt = dist_u + length

                    if (alt < dist_v) {
                      prev.set(vRow, vCol, dist.pack(uRow, uCol))
                      dist.set(vRow, vCol, alt)
                    }
                  }
                }

                colOffset += 1
              }
              rowOffset += 1
            }
          }
        }
        (dist, prev)
      }

      def replicate(labyrinth: Matrix.Typ[Int], times: Int) = {
        val newMt =
          Matrix.create[Int](labyrinth.width * times, labyrinth.height * times)

        loops.loop(0, labyrinth.maxRow) { row =>
          loops.loop(0, labyrinth.maxCol) { col =>
            loops.loop(0, times - 1) { replicaVertical =>
              loops.loop(0, times - 1) { replicaHorizontal =>
                val newRow = replicaVertical * labyrinth.height + row
                val newCol = replicaHorizontal * labyrinth.width + col
                val append = replicaHorizontal + replicaVertical
                val newStuff = (labyrinth.unsafe(row, col) + append) % 9
                val newValue = if (newStuff == 0) 9 else newStuff

                newMt.set(newRow, newCol, newValue)

              }
            }
          }
        }

        newMt

      }

      def print(
          matrix: Matrix.Typ[Int],
          distances: Matrix.Typ[Int],
          highlight: Bitset.Typ
      ) = {
        import Bitset._
        val red = toCString(Console.RED)
        val yello = toCString(Console.YELLOW)
        val green = toCString(Console.CYAN + Console.BOLD)
        val reset = toCString(Console.RESET)
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
            stdio.printf(c"%4d", distances.unsafe(row, col))
            if (highlight.get(elId))
              stdio.printf(green)
            else
              stdio.printf(red)
            stdio.printf(c"|%1d", matrix.unsafe(row, col))
            stdio.printf(reset)

          }
          stdio.printf(c"\n")
        }
      }
      def findOptimal(prev: Matrix.Typ[Int]): Bitset.Typ = {
        import Bitset._
        val optimal = Bitset.create(prev.width * prev.height)
        var u = prev.pack(0, 0)
        val UNDEFINED = -1

        while (u != UNDEFINED) {
          optimal.set(u)
          u = prev.unsafe(
            prev.unpackRow(u),
            prev.unpackCol(u)
          )
        }

        optimal
      }

      val part_1_answer = {
        val matrix = readInput(args.head)
        val (results, prev) = dijkstra(matrix)

        // if (matrix.width < 100)
        //   print(matrix, results, findOptimal(prev))

        results.unsafe(0, 0)
      }
      println(s"Part 1: ${part_1_answer}")

      val part_2_answer = {
        val matrix = readInput(args.head)
        val large = replicate(matrix, 5)
        val (dist, prev) = dijkstra(large)

        dist.unsafe(0, 0)
      }

      println(s"Part 2: ${part_2_answer}")
    }
  }
}
