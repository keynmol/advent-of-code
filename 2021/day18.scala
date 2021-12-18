// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.annotation.tailrec
import scala.scalanative.annotation.alwaysinline
import scala.scalanative.runtime.Boxes
import scala.scalanative.runtime.Intrinsics
import scala.scalanative.unsafe._

import scalanative.libc.stdio

object Day18 {

  type Pair = CStruct4[Boolean, Long, Boolean, Long]
  implicit class PairOpts(p: Pair) {
    @alwaysinline def isLeftSimple = p._1 == false
    @alwaysinline def isRightSimple = p._3 == false

    def leftNumber = p._2.toInt
    def rightNumber = p._4.toInt

    def leftPair: Ptr[Pair] =
      Boxes.boxToPtr[Pair](Intrinsics.castLongToRawPtr(p._2))
    def rightPair: Ptr[Pair] =
      Boxes.boxToPtr[Pair](Intrinsics.castLongToRawPtr(p._4))

    def setLeft(pair: Pair) = {
      p._1 = true
      p._2 = pair.toPtr.toLong
    }
    def setRight(pair: Pair) = {
      p._3 = true
      p._4 = pair.toPtr.toLong
    }
    def setLeft(num: Long) = {
      p._1 = false
      p._2 = num
    }
    def setRight(num: Long) = {
      p._3 = false
      p._4 = num
    }

    def print: Unit = {
      stdio.printf(c"[")
      if (isLeftSimple) {
        stdio.printf(c"%d", leftNumber)
      } else {
        (!leftPair).print
      }

      stdio.printf(c",")
      if (isRightSimple) {
        stdio.printf(c"%d", rightNumber)
      } else {
        (!rightPair).print
      }

      stdio.printf(c"]")
    }
    def printDetailed: Unit = {
      def go(pair: Pair, level: Int): Unit = {
        def printLevel = loops.loop(1, level) { _ => stdio.printf(c".") }
        printLevel
        stdio.printf(c"[\n")
        if (pair.isLeftSimple) {
          printLevel
          stdio.printf(c"%d\n", pair.leftNumber)
        } else {
          go(!pair.leftPair, level + 1)
          // (!leftPair).print
        }
        printLevel
        stdio.printf(c",\n")
        if (pair.isRightSimple) {
          printLevel
          stdio.printf(c"%d\n", pair.rightNumber)
        } else {
          go(!pair.rightPair, level + 1)
        }

        stdio.printf(c"\n]")
      }

      go(p, 0)
    }

    def magnitude: Long = {
      var result = 0L

      if (isLeftSimple) result += 3 * leftNumber
      else result += 3 * (!leftPair).magnitude

      if (isRightSimple) result += 2 * rightNumber
      else result += 2 * (!rightPair).magnitude

      result
    }
    def copy(implicit z: Zone): Pair = {
      val mem = alloc[Pair](1)
      if (isLeftSimple) {
        mem._1 = false
        mem._2 = leftNumber
      } else {
        mem._1 = true
        mem._2 = (!leftPair).copy.toPtr.toLong
      }
      if (isRightSimple) {
        mem._3 = false
        mem._4 = rightNumber
      } else {
        mem._3 = true
        mem._4 = (!rightPair).copy.toPtr.toLong
      }

      mem
    }
  }

  object Pair {
    def regular(l: Int, r: Int)(implicit z: Zone): Pair = {
      val mem = alloc[Pair](1)
      mem._1 = false
      mem._2 = l
      mem._3 = false
      mem._4 = r

      mem
    }
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      def parsePair(p: parser.Parser): Pair = {
        val mem = alloc[Pair](1)
        p.const(c"[")()
        if (p.remainingString(0) == '[') {
          val nested = parsePair(p)
          (!mem).setLeft(nested)
        } else {
          mem._1 = false
          p.long(mem.at2)
        }
        p.const(c",")()

        if (p.remainingString(0) == '[') {
          val nested = parsePair(p)
          (!mem).setRight(nested)
        } else {
          mem._3 = false
          p.long(mem.at4)
        }

        p.const(c"]")()

        mem

      }

      def reduce1(pair: Pair) = {
        val stack = Stack.create[Ptr[Pair]]

        def go(in: Ptr[Pair], level: Int): Boolean = {
          if (level == 4) { stack.push(in); true }
          else {
            stack.push(in)
            val foundInLeft =
              if (!(!in).isLeftSimple) go((!in).leftPair, level + 1) else false
            val foundInRight = if (!foundInLeft) {
              if (!(!in).isRightSimple) go((!in).rightPair, level + 1)
              else false
            } else false

            if (!foundInLeft && !foundInRight) {
              stack.pop()
              false
            } else true

          }

        }

        go(pair.toPtr, 0)
        val deep = WrappedArray.create[Ptr[Pair]]()

        while (!stack.isEmpty) deep.appendAndGrow(stack.pop())

        if (deep.size == 5) {

          val explodingPtr = !deep.at(0)
          val containerPtr = !deep.at(1)

          val exploding = !explodingPtr
          val container = !containerPtr

          def add(n: Int, pair: Pair, goLeft: Boolean): Unit = {
            if (pair.isLeftSimple && goLeft)
              pair.setLeft(n + pair.leftNumber)
            if (pair.isRightSimple && !goLeft)
              pair.setRight(n + pair.rightNumber)
            if (!pair.isLeftSimple && goLeft) add(n, !pair.leftPair, goLeft)
            if (!pair.isRightSimple && !goLeft) add(n, !pair.rightPair, goLeft)
          }

          val onTheLeft = container.leftPair == explodingPtr
          val onTheRight = !onTheLeft

          // #1 change leftmost number
          if (onTheRight) {
            if (container.isLeftSimple)
              container.setLeft(container.leftNumber + exploding.leftNumber)
            else add(exploding.leftNumber, container.leftPair, goLeft = false)
          } else {
            var i = 0
            var found = false
            while (i < 4 && !found) {
              val currentPtr = !deep.at(i)
              val prevPtr = !(deep.at(i + 1))
              if ((!prevPtr).rightPair == currentPtr)
                found = true
              i += 1
            }
            if (found) {
              val container = !(!(deep.at(i)))
              if (container.isLeftSimple)
                container.setLeft(container.leftNumber + exploding.leftNumber)
              else add(exploding.leftNumber, container.leftPair, goLeft = false)
            }
          }

          // #2 change rightmost number
          if (onTheLeft) {
            if (container.isRightSimple)
              container.setRight(container.rightNumber + exploding.rightNumber)
            else add(exploding.rightNumber, container.rightPair, goLeft = true)
          } else {
            var i = 0
            var found = false
            while (i < 4 && !found) {
              val currentPtr = !(deep.at(i))
              val prevPtr = !(deep.at(i + 1))
              if ((!prevPtr).leftPair == currentPtr)
                found = true
              i += 1
            }

            if (found) {
              val container = !(!(deep.at(i)))
              if (container.isRightSimple)
                container.setRight(
                  container.rightNumber + exploding.rightNumber
                )
              else
                add(exploding.rightNumber, container.rightPair, goLeft = true)
            }
          }

          // #3 Replace with 0
          if (onTheLeft)
            container.setLeft(0)
          else container.setRight(0)

        }
        deep.size == 5
      }

      def reduce2(pair: Pair) = {
        var hasSplit = false

        def go(in: Pair): Unit = {
          if (!hasSplit) {
            if (!in.isLeftSimple) go(!in.leftPair)
            else if (in.leftNumber >= 10) {
              in.setLeft(split(in.leftNumber))
              hasSplit = true
            }

            if (!hasSplit) {
              if (!in.isRightSimple) go(!in.rightPair)
              else if (in.rightNumber >= 10) {
                in.setRight(split(in.rightNumber))
                hasSplit = true
              }
            }
          }
        }

        go(pair)

        hasSplit
      }

      def parseString(l: CString): Pair = {
        parsePair(parser.init(l))
      }

      def add(pair1: Pair, pair2: Pair): Pair = {
        val mem = alloc[Pair](1)
        (!mem).setLeft(pair1)
        (!mem).setRight(pair2)

        continuousReduce(mem)
        mem
      }

      def split(num: Int): Pair = {
        if (num % 2 == 0) Pair.regular(num / 2, num / 2)
        else {
          Pair.regular((num - 1) / 2, (num + 1) / 2)
        }
      }

      @tailrec
      def continuousReduce(pair: Pair): Unit = {
        var hasExploded = false
        while ({ hasExploded = reduce1(pair); hasExploded }) {}
        val hasSplit = reduce2(pair)

        if (hasExploded || hasSplit) continuousReduce(pair)
      }

      val part_1_answer = {
        var result: Pair = null
        files.parsedLines(args.head) { case (l, p) =>
          val pair = parsePair(p)
          if (result == null) result = pair
          else
            result = add(result, pair)

        }

        result.magnitude
      }

      val part_2_answer = {
        val pairs = WrappedArray.create[Pair]()
        files.parsedLines(args.head) { case (l, p) =>
          val pair = parsePair(p)
          pairs.appendAndGrow(pair)
        }
        var maxMagnitude = -1L
        loops.loop(1, pairs.size) { i =>
          loops.loop(1, pairs.size) { j =>
            if (i != j) {
              val first = !(pairs.at(i - 1))
              val second = !(pairs.at(j - 1))
              val sum = add(first.copy, second.copy)

              if (sum.magnitude > maxMagnitude) maxMagnitude = sum.magnitude
            }
          }
        }

        maxMagnitude

      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
