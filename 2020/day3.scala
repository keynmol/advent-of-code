// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string

object Day3 {
  case class World(trees: WrappedArray[Boolean], width: Int, height: Int) {
    def at(right: Int, down: Int) = {
      val safeRight = right % width
      !trees.at(down * width + safeRight)
    }
  }
  def traverse(world: World, right: Int, down: Int) = {
    var encountered = 0L
    for (i <- 0 until world.height) {
      if (down * i < world.height)
        if (world.at(right = right * i, down = down * i)) encountered += 1
    }

    encountered
  }

  def solve_part_1(world: World) = {
    traverse(world, 3, 1)
  }

  def solve_part_2(world: World) = {
    traverse(world, 1, 1) *
      traverse(world, 3, 1) *
      traverse(world, 5, 1) *
      traverse(world, 7, 1) *
      traverse(world, 1, 2)
  }

  def main(args: Array[String]): Unit = {
    val MAX_LINE_LENGTH = 100
    Zone.apply { implicit z =>
      val filename = toCString(args.head)
      val file = stdio.fopen(filename, c"r")
      val line = stackalloc[CChar](MAX_LINE_LENGTH)
      val from = stackalloc[CInt]
      val to = stackalloc[CInt]
      val char = stackalloc[CChar]
      val str = stackalloc[CChar](MAX_LINE_LENGTH)
      var valid_policy_1 = 0
      var valid_policy_2 = 0
      val trees = WrappedArray.create[Boolean]()
      var patternWidth = 0
      var patternHeight = 0
      while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
        patternWidth = string.strlen(line).toInt - 1 // remove the \n
        patternHeight += 1
        strings.foreachCharWithIndex(line) { (c, i) =>
          if (c != '\n') trees.appendAndGrow(c == '#')
        }
      }

      val world = World(trees, patternWidth, patternHeight)

      println(s"Part 1: ${solve_part_1(world)}")
      println(s"Part 2: ${solve_part_2(world)}")

      stdio.fclose(file)
    }
  }
}
