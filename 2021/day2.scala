// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._

object Day2 {

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val num = stackalloc[Int]

      val part_1_answer = {
        var horizontal = 0
        var depth = 0
        files.parsedLines(args.head) { case (line, parser) =>
          val action = line(0)
          if (action == 'f') parser.const(c"forward")().space().int(num)
          else if (action == 'd') parser.const(c"down")().space().int(num)
          else if (action == 'u') parser.const(c"u")().space().int(num)

          action match {
            case 'f' => horizontal += !num
            case 'u' => depth -= !num
            case 'd' => depth += !num
          }
        }

        depth * horizontal
      }

      val part_2_answer = {
        var horizontal = 0
        var aim = 0
        var depth = 0
        files.parsedLines(args.head) { case (line, parser) =>
          val action = line(0)
          if (action == 'f') parser.const(c"forward")().space().int(num)
          else if (action == 'd') parser.const(c"down")().space().int(num)
          else if (action == 'u') parser.const(c"u")().space().int(num)

          action match {
            case 'f' => horizontal += !num; depth += aim * !num
            case 'u' => aim -= !num
            case 'd' => aim += !num
          }
        }

        horizontal * depth
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
