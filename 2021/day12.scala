// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string
import scala.scalanative.annotation.alwaysinline

object Day12 {

  def indexOf(
      cstr: CString,
      nodes: WrappedArray[CString],
      cmpLength: CSize
  ) = {
    var found = -1
    nodes.foreach.breakableWithIndex(stopWhen = found != -1) {
      case (str, idx) =>
        if (string.strncmp(str, cstr, cmpLength) == 0)
          found = idx
    }
    found
  }

  def pack(row: Int, col: Int) = {
    (row << 16) + col
  }

  @alwaysinline def unpackRow(dense: Int) = {
    dense >>> 16
  }

  @alwaysinline def unpackCol(dense: Int) = {
    (dense << 16) >>> 16
  }

  import Matrix._

  def readInput(filename: String, nodes: WrappedArray[CString])(implicit
      z: Zone
  ): Matrix.Typ[Int] = {
    val zips = WrappedArray.create[Int]()
    files.lines(filename) { l =>
      val len = string.strlen(l).toInt
      val hyphenLocation = {
        var found = -1
        loops.breakable(0, len, stopWhen = found != -1) { idx =>
          if (l(idx) == '-') found = idx
        }
        found
      }

      var fromIdx = indexOf(l, nodes, hyphenLocation.toULong)
      var toIdx = indexOf(
        l + hyphenLocation + 1,
        nodes,
        (len - hyphenLocation - 2).toULong
      )

      if (fromIdx == -1) {
        val newStr = alloc[CChar](hyphenLocation)
        string.strncpy(newStr, l, hyphenLocation.toULong)
        nodes.appendAndGrow(newStr)
        fromIdx = nodes.size - 1
      }
      if (toIdx == -1) {
        val newStr = alloc[CChar](len - hyphenLocation - 1)
        string.strncpy(
          newStr,
          l + hyphenLocation + 1,
          (len - hyphenLocation - 2).toULong
        )
        nodes.appendAndGrow(newStr)
        toIdx = nodes.size - 1
      }

      zips.appendAndGrow(pack(fromIdx, toIdx))

    }

    val mt = alloc[Int](nodes.size * nodes.size)
    val struct = alloc[Matrix.Typ[Int]]

    struct._2 = nodes.size
    struct._3 = nodes.size
    struct._1 = mt

    zips.foreach { i =>
      val from = unpackRow(i)
      val to = unpackCol(i)
      mt(from * nodes.size + to) = 1
      mt(to * nodes.size + from) = 1

    }
    struct
  }

  def isBigCave(idx: Int, nodes: WrappedArray[CString]): Boolean = {
    val str = !(nodes.at(idx))
    var isBig = true
    loops.breakable(0, string.strlen(str).toInt, stopWhen = !isBig) { idx =>
      isBig = str(idx).toInt <= 90
    }

    isBig
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val num = stackalloc[Int]

      val part_1_answer = {
        var answer = 0
        val nodes = WrappedArray.create[CString]()
        val adjacency = readInput(filename = args.head, nodes)
        val start = indexOf(c"start", nodes, string.strlen(c"start"))
        val end = indexOf(c"end", nodes, string.strlen(c"end"))
        import Bitset._

        def go(nodeIdx: Int, visited: Bitset.Typ, level: Int): Int = {
          def printLevel = loops.loop(1, level) { _ => stdio.printf(c"-") }
          visited.set(nodeIdx)
          var paths = 0
          if (nodeIdx == end) paths = 1
          else {

            loops.loop(0, nodes.size - 1) { other =>
              val edgeExists = adjacency.at(nodeIdx, other, 100) == 1

              val canGo = nodeIdx != other &&
                (!visited.get(other) || isBigCave(
                  other,
                  nodes
                ))
              if (edgeExists && canGo) {
                paths += go(other, visited.copy, level + 1)
              }
            }
          }

          paths
        }

        go(start, Bitset.create(nodes.size), 1)
      }

      val part_2_answer = {
        val nodes = WrappedArray.create[CString]()
        val adjacency = readInput(filename = args.head, nodes)
        val start = indexOf(c"start", nodes, string.strlen(c"start"))
        val end = indexOf(c"end", nodes, string.strlen(c"end"))
        val bigcaves = Bitset.create(nodes.size)
        import Bitset._
        loops.loop(1, nodes.size) { i =>
          if (isBigCave(i - 1, nodes)) bigcaves.set(i)
        }
        def alreadyVisitedASmallCaveTwice(visited: Bitset.Typ): Boolean = {
          var found = -1
          loops.breakable(1, nodes.size, stopWhen = found != -1) { nodeId =>
            val nodeIdx = nodeId - 1
            if (
              !bigcaves.get(nodeId) &&
              visited.get(nodeId) &&
              visited.get(nodeId + nodes.size)
            )
              found = nodeId

          }
          found != -1
        }

        def go(nodeIdx: Int, visited: Bitset.Typ, level: Int): Int = {

          val nodeId = nodeIdx + 1
          val secondVisitNodeId = nodeId + nodes.size

          if (visited.get(nodeId))
            visited.set(secondVisitNodeId)
          visited.set(nodeId)
          var paths = 0
          if (nodeIdx == end) paths = 1
          else {
            loops.loop(0, nodes.size - 1) { other =>
              val edgeExists = adjacency.at(nodeIdx, other, 100) == 1
              val otherNodeId = other + 1
              val otherSecondVisitNodeId = otherNodeId + nodes.size
              val notVisited =
                !visited.get(otherNodeId) ||
                  (!alreadyVisitedASmallCaveTwice(visited) &&
                    !visited.get(
                      otherSecondVisitNodeId
                    ))
              val bigCave = bigcaves.get(otherNodeId)
              val differentNode = nodeIdx != other

              val canGo =
                edgeExists && differentNode && other != start &&
                  (bigCave || notVisited)
              if (canGo) {
                paths += go(other, visited.copy, level + 1)
              }
            }
          }

          paths
        }

        go(start, Bitset.create(nodes.size * 2), 1)
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
