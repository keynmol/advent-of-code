// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string
import scala.scalanative.annotation.alwaysinline

object Day4 {
  val SIZE = 5
  val MARKER = Int.MaxValue

  def readCalledOutNumbers(line: CString, ar: WrappedArray[Int])(implicit
      z: Zone
  ) = {
    val p = parser.init(line)
    val num = stackalloc[Int]

    while (string.strcmp(p.remainingString, c"\n") != 0) {
      p.int(num).const(c",")()

      ar.appendAndGrow(!num)
    }
  }

  def readBoardLine(line: CString, row: Int, board: Ptr[Int])(implicit
      z: Zone
  ) = {
    var col = 0
    val p = parser.init(line)
    val num = stackalloc[Int]
    while (string.strcmp(p.remainingString, c"\n") != 0) {
      p.int(num).const(c" ")()
      board(row * SIZE + col) = !num
      col += 1
    }
  }

  def printBoard(board: Ptr[Int]) = {
    loops.loop(0, SIZE - 1) { row =>
      loops.loop(0, SIZE - 1) { col =>
        if (board(row * SIZE + col) == MARKER)
          stdio.printf(c"X ")
        else
          stdio.printf(c"%d ", board(row * SIZE + col))
      }
      stdio.printf(c"\n")
    }
  }

  def readTheGame(
      filename: String,
      numbers: WrappedArray[Int],
      boards: WrappedArray[Ptr[Int]]
  )(implicit z: Zone) = {
    var board: Ptr[Int] = null
    var row = 0
    files.parsedLinesWithIndex(filename, maxLineLength = 500) {
      case (line, parser, idx) =>
        if (idx == 0) readCalledOutNumbers(line, numbers)
        else if (line(0) == '\n') {
          if (board != null) {
            boards.appendAndGrow(board)
          }
          board = alloc[Int](SIZE * SIZE)
          row = 0
        } else {
          readBoardLine(line, row, board)
          row += 1
        }
    }
    if (board != null) { boards.appendAndGrow(board) }
  }

  @alwaysinline def checkRow(board: Ptr[Int], row: Int): Boolean = {
    var bingo = true
    loops.loop(0, SIZE - 1) { col =>
      bingo = bingo && board(row * SIZE + col) == MARKER
    }
    bingo
  }

  @alwaysinline def checkCol(board: Ptr[Int], col: Int): Boolean = {
    var bingo = true
    loops.loop(0, SIZE - 1) { row =>
      bingo = bingo && board(row * SIZE + col) == MARKER
    }
    bingo
  }

  @alwaysinline def checkBingo(board: Ptr[Int]) = {
    var bingo = false

    loops.breakable(0, SIZE - 1, stopWhen = bingo) { dim =>
      bingo = checkRow(board, dim) || checkCol(board, dim)
    }

    bingo
  }

  def findWinner(boards: WrappedArray[Ptr[Int]]): Int = {
    var winner = -1

    boards.foreach.breakableWithIndex(winner != -1) { case (board, idx) =>
      if (checkBingo(board)) winner = idx
    }

    winner
  }

  def recordWinners(
      boards: WrappedArray[Ptr[Int]],
      skip: WrappedArray[Int],
      scores: WrappedArray[Int]
  )(implicit z: Zone) = {
    var winners = 0

    boards.foreach.withIndex { case (board, idx) =>
      val skippable = {
        var result = false
        skip.foreach { i =>
          result = result || i == idx
        }
        result
      }
      if (!skippable && checkBingo(board)) {
        skip.appendAndGrow(idx)
        scores.appendAndGrow(boardScore(board))
        winners += 1
      }
    }

    winners
  }

  def drawNumber(
      drawn: Int,
      boards: WrappedArray[Ptr[Int]]
  ) = {
    boards.foreach { board =>
      loops.loop(0, SIZE * SIZE - 1) { idx =>
        if (board(idx) == drawn) board(idx) = MARKER //-drawn
      }
    }
  }

  def boardScore(board: Ptr[Int]): Int = {
    var sum = 0
    loops.loop(0, SIZE - 1) { row =>
      loops.loop(0, SIZE - 1) { col =>
        val n = board(row * SIZE + col)
        if (n != MARKER) sum += n
      }
    }

    sum
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val part_1_answer = {
        var answer = 0
        val numbers = WrappedArray.create[Int]()
        val boards = WrappedArray.create[Ptr[Int]]()
        readTheGame(args.head, numbers, boards)

        var winner = -1

        numbers.foreach.breakable(stopWhen = winner != -1) { drawn =>
          drawNumber(drawn, boards)
          winner = findWinner(boards)
          if (winner != -1)
            answer = drawn * boardScore(!boards.at(winner))

        }

        answer
      }

      val part_2_answer = {
        var answer = 0
        val numbers = WrappedArray.create[Int]()
        val boards = WrappedArray.create[Ptr[Int]]()
        val skip = WrappedArray.create[Int]()
        val scores = WrappedArray.create[Int]()
        readTheGame(args.head, numbers, boards)

        numbers.foreach { drawn =>
          drawNumber(drawn, boards)
          val winners = recordWinners(boards, skip, scores)
          if (winners != 0) {
            answer = drawn
          }
        }

        answer * !scores.at(skip.size - 1)
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
