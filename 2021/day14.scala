// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.annotation.alwaysinline

object Day14 {
  type Fold = CStruct3[CChar, CChar, CChar]
  implicit class FoldOps(f: Fold) {
    def from1 = f._1
    def from2 = f._2
    def insert = f._3
  }

  def readInput(
      filename: String,
      polymer: CString,
      folds: WrappedArray[Fold]
  )(implicit z: Zone) = {
    var reading_folds = false

    files.parsedLines(filename) { case (_, parser) =>
      if (parser.const(c"\n")().successful)
        reading_folds = true
      else {
        if (!reading_folds)
          parser.string(polymer)
        else {
          val fold = alloc[Fold](1)
          parser.char(fold.at1).char(fold.at2).const(c" -> ")().char(fold.at3)

          folds.appendAndGrow(fold)
        }
      }
    }
  }

  def solve(start: CString, folds: WrappedArray[Fold], STEPS: Int)(implicit z: Zone): Long = {
    import Matrix._
    val data = alloc[Long](26 * 26)
    var mt: Matrix.Typ[Long] = alloc[Matrix.Typ[Long]](1)

    mt._1 = data
    mt._2 = 26
    mt._3 = 26

    @alwaysinline def coord(c: CChar) = c - 'A'

    def countEverything(matrix: Matrix.Typ[Long]) = {
      var maxCount = -1L
      var minCount = Long.MaxValue
      loops.loop(0, 25) { i =>
        val horizontalCount = {
          var result = 0L
          loops.loop(0, 25) { col =>
            result += matrix.unsafe(i, col)
          }
          result
        }
        val verticalCount = {
          var result = 0L
          loops.loop(0, 25) { row =>
            result += matrix.unsafe(row, i)
          }
          result
        }

        val count =
          if (horizontalCount > verticalCount) horizontalCount
          else verticalCount

        if (count > 0) {
          if (count > maxCount) maxCount = count
          if (count < minCount) minCount = count
        }
      }

      maxCount - minCount
    }

    val len = strings.len(start)
    strings.foreachCharWithIndex(start) { case (char, idx) =>
      if (idx != len - 1) {
        val next = start(idx + 1)
        mt.set(
          coord(char),
          coord(next),
          mt.at(coord(char), coord(next), 100L) + 1
        )
      }
    }

    def get(matrix: Matrix.Typ[Long], row: CChar, col: CChar): Long = {
      matrix.unsafe(coord(row), coord(col))
    }
    def set(
        matrix: Matrix.Typ[Long],
        row: CChar,
        col: CChar,
        value: Long
    ) = {
      matrix.set(coord(row), coord(col), value)
    }

    loops.loop(1, STEPS) { _ =>
      val inProgress = mt.copy
      folds.foreach { fold =>
        val occurrences = get(mt, fold.from1, fold.from2)
        set(
          inProgress,
          fold.from1,
          fold.insert,
          occurrences + get(inProgress, fold.from1, fold.insert)
        )
        set(
          inProgress,
          fold.insert,
          fold.from2,
          occurrences + get(inProgress, fold.insert, fold.from2)
        )

        val diff = get(inProgress, fold.from1, fold.from2) - occurrences
        if (diff > 0)
          set(
            inProgress,
            fold.from1,
            fold.from2,
            diff
          )
        else set(inProgress, fold.from1, fold.from2, 0)
      }

      mt = inProgress
    }

    countEverything(mt)
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val part_1_answer = {
        val polymer = alloc[CChar](100)
        val folds = WrappedArray.create[Fold]()
        readInput(args.head, polymer, folds)
        solve(polymer, folds, 10)
      }

      val part_2_answer = {
        val polymer = alloc[CChar](100)
        val folds = WrappedArray.create[Fold]()
        readInput(args.head, polymer, folds)
        solve(polymer, folds, 40)
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
