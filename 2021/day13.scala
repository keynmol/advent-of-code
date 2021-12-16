// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
// import scala.scalanative.unsigned._
import scalanative.libc.stdio

object Day13 {
  import Matrix._
  type Coord = CStruct2[Int, Int]
  type Fold = Int // make them opaque once Scala 3 lands

  def readInput(
      filename: String,
      coords: WrappedArray[Coord],
      folds: WrappedArray[Fold]
  )(implicit z: Zone): Matrix.Typ[Int] = {
    var reading_coordinates = true
    val foldAmount = stackalloc[Int](1)
    var maxHoriz = -1
    var maxVert = -1
    files.parsedLines(filename) { case (_, parser) =>
      if (parser.const(c"\n")().successful)
        reading_coordinates = false
      else if (reading_coordinates) {
        val pair = stackalloc[Coord](1)

        parser.int(pair.at1).const(c",")().int(pair.at2)

        if (pair._1 > maxHoriz) maxHoriz = pair._1
        if (pair._2 > maxVert) maxVert = pair._2

        coords.appendAndGrow(pair)
      } else {
        parser.const(c"fold along ")()
        val isHorizontal = parser.remainingString(0) == 'x'
        if (isHorizontal)
          parser.const(c"x=")().int(foldAmount)
        else
          parser.const(c"y=")().int(foldAmount)

        if (isHorizontal) folds.appendAndGrow(!foldAmount)
        else folds.appendAndGrow(-1 * !foldAmount)
      }

    }
    val data = alloc[Int]((maxHoriz + 1) * (maxVert + 1))
    val mt: Matrix.Typ[Int] = alloc[Matrix.Typ[Int]](1)

    mt._1 = data
    mt._2 = maxHoriz + 1
    mt._3 = maxVert + 1
    loops.loop(0, maxHoriz) { col =>
      loops.loop(0, maxVert) { row =>
        mt.set(row, col, 0)
      }
    }

    coords.foreach { cord =>
      data(cord._2 * mt._2 + cord._1) = 1
    }

    mt
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val num = stackalloc[Int]

      val part_1_answer = {
        var answer = 0
        val coords = WrappedArray.create[Coord]()
        val folds = WrappedArray.create[Int]()
        val matrix = readInput(args.head, coords, folds)

        var right = matrix.maxCol
        var bottom = matrix.maxRow

        def printMatrix = {

          loops.loop(0, bottom) { row =>
            stdio.printf(c"%3d:  ", row)
            loops.loop(0, right) { col =>
              if (matrix.at(row, col, 100) == 1) {
                stdio.printf(c"#")
              } else stdio.printf(c".")
            }
            stdio.printf(c"\n")

          }
        }
        def countMatrix = {
          var dots = 0
          loops.loop(0, bottom) { row =>
            loops.loop(0, right) { col =>
              if (matrix.at(row, col, 100) == 1)
                dots += 1
              else if (matrix.at(row, col, 100) == 100) {
                println(s"wuuuut $row $col")
              }
            }

          }
          dots
        }

        folds.foreach { fold =>
          stdio.printf(c"Dimenstions: bottom=%d, right=%d\n", bottom, right)
          val isHorizontal = fold > 0
          val foldAmount = if (isHorizontal) fold else -fold
          val oldBottom = bottom
          val oldRight = right
          if (isHorizontal) {
            right = foldAmount - 1

          } else {
            bottom = foldAmount - 1
          }
          stdio.printf(
            c"Dimenstions (after applying fold %d): bottom=%d, right=%d\n",
            fold,
            bottom,
            right
          )
          if (!isHorizontal) {
            loops.loop(foldAmount + 1, oldBottom) { removedRow =>
              val distance = removedRow - (foldAmount + 1)
              val row = bottom - distance

              // stdio.printf(c"Row %d is decided based on %d\n", row, removedRow)

              loops.loop(0, right) { col =>
                if (matrix.at(removedRow, col, 100) == 1)
                  matrix.set(row, col, 1)
              }
            }
          } else {
            loops.loop(foldAmount + 1, oldRight) { removedCol =>
              val distance = removedCol - (foldAmount + 1)
              val col = right - distance

              // stdio.printf(c"Col %d is decided based on %d\n", col, removedCol)
              loops.loop(0, bottom) { row =>
                if (matrix.at(row, removedCol, 100) == 1)
                  matrix.set(row, col, 1)
              }
            }
          }
          // printMatrix
          stdio.printf(c"Dots: %d after %d\n", countMatrix, fold)
        }

        printMatrix

      }

      val part_2_answer = {
        var answer = 0
        files.linesWithIndex(args.head) { case (line, idx) =>
          stdio.sscanf(line, c"%d\n", num)
        }

        answer
      }

    // println(s"Part 1: ${part_1_answer}")
    // println(s"Part 2: ${part_2_answer}")
    }
  }
}
