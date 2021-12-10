// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.libc.stdlib

object Day10 {

  def main(args: Array[String]): Unit = {
    def matching(bracket: CChar): CChar = bracket match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
      case ')' => '('
      case ']' => '['
      case '}' => '{'
      case '>' => '<'
    }
    Zone.apply { implicit z =>
      val part_1_answer = {
        var answer = 0
        val stack = Stack.create[CChar]
        files.lines(args.head) { line =>
          var illegal: CChar = -1
          strings.foreachChar(line) { char =>
            char match {
              case '(' | '[' | '{' | '<' =>
                stack.push(char)
              case '\n' =>
              case other =>
                val opening = matching(other)
                val st = stack.pop()
                if (opening != st && illegal == -1) {
                  illegal = other
                }
            }

          }
          answer += {
            illegal match {
              case ')' => 3
              case ']' => 57
              case '}' => 1197
              case '>' => 25137
              case _   => 0
            }
          }
        }

        answer
      }

      val part_2_answer = {
        val stack = Stack.create[CChar]
        val scores = WrappedArray.create[Long]()
        files.lines(args.head) { line =>
          var illegal: CChar = -1
          stack.reset()
          strings.foreachChar(line) { char =>
            char match {
              case '(' | '[' | '{' | '<' =>
                stack.push(char)
              case '\n' =>
              case other =>
                val opening = matching(other)
                val st = stack.pop()
                if (opening != st && illegal == -1) {
                  illegal = other
                }
            }

          }
          if (illegal == -1) {
            var score = 0L
            while (!stack.isEmpty) {
              val bump = matching(stack.pop()) match {
                case ')' => 1
                case ']' => 2
                case '}' => 3
                case '>' => 4
              }

              score = score * 5 + bump
            }

            scores.appendAndGrow(score)
          }
        }

        val cont = stackalloc[Long](scores.size)
        scores.continuous(cont)
        stdlib.qsort(
          cont.asInstanceOf[Ptr[Byte]],
          scores.size.toULong,
          sizeof[Long],
          CFuncPtr2.fromScalaFunction { case (a1, a2) =>
            val diff =
              (!(a1.asInstanceOf[Ptr[Long]]) - !(a2.asInstanceOf[Ptr[Long]]))
            if (diff < 0) -1 else 1
          }
        )

        cont((scores.size - 1) / 2)
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
