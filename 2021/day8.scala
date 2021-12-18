// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.libc.string
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._

object Day8 {

  val size = 7

  def toBin(str: CString): Int = {
    var numb = 0
    strings.foreachChar(str) { char =>
      val shift = 1 << (size - 1 - (char - 'a'))

      numb += shift
    }

    numb
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val part_1_answer = {
        var answer = 0
        val word = stackalloc[CChar](7)
        files.parsedLines(args.head) { case (_, parser) =>
          while (string.strncmp(parser.remainingString, c"| ", 2.toUInt) != 0) {
            val word = stackalloc[CChar](7)
            parser.string(word).const(c" ")()
          }

          parser.const(c"|")()
          while (string.strcmp(parser.remainingString, c"\n") != 0) {
            parser.const(c" ")().string(word)

            val len = string.strlen(word).toInt
            len match {
              case 2 | 3 | 4 | 7 => answer += 1
              case _             =>
            }
          }

        }

        answer
      }

      def extract(position: Int, rec: Ptr[Int]): Int = {
        var num = 0
        loops.loop(0, 9) { i =>
          val mask = 1 << (size - 1 - position)
          val add = 1 << (9 - i)

          if ((mask & rec(i)) != 0) num += add
        }

        num
      }

      val part_2_answer = {
        var answer = 0
        val blueprint = stackalloc[Int](10)
        blueprint(1) = toBin(c"cf")
        blueprint(7) = toBin(c"acf")
        blueprint(4) = toBin(c"bcdf")

        blueprint(2) = toBin(c"acdeg")
        blueprint(3) = toBin(c"acdfg")
        blueprint(5) = toBin(c"abdfg")

        blueprint(6) = toBin(c"abdefg")
        blueprint(0) = toBin(c"abcefg")
        blueprint(9) = toBin(c"abcdfg")

        blueprint(8) = toBin(c"abcdefg")

        val records = stackalloc[Int](10)
        files.parsedLines(args.head) { case (_, p) =>
          val signals = WrappedArray.create[CString](10)
          while (string.strncmp(p.remainingString, c"| ", 2.toUInt) != 0) {
            val word = stackalloc[CChar](7)
            p.string(word).const(c" ")()
            val len = string.strlen(word).toInt
            len match {
              case 2 => records(1) = toBin(word)
              case 3 => records(7) = toBin(word)
              case 4 => records(4) = toBin(word)
              case 7 => records(8) = toBin(word)
              case _ =>
            }
            signals.appendAndGrow(word)
          }

          signals.foreach { word =>
            val len = string.strlen(word).toInt
            len match {
              case 6 =>
                val binString = toBin(word)

                val is6 = (binString & records(1)) != records(1)
                val is9 = (binString & records(4)) == records(4)
                if (is6) records(6) = binString
                else if (is9) records(9) = binString
                else records(0) = binString

              case 5 =>
                val binString = toBin(word)
                val is2 = {

                  ((binString | records(1)) & records(4)) != records(4)
                }
                val is3 = (binString & records(1)) == records(1)
                if (is3) records(3) = binString
                else if (is2) records(2) = binString
                else records(5) = binString
              case _ =>
            }
          }
          val mapping = stackalloc[Int](7)

          loops.loop(0, size - 1) { let =>
            val column = extract(let, blueprint)
            var found = -1
            loops.breakable(0, size - 1, stopWhen = found != -1) { scr =>
              if (extract(scr, records) == column) found = scr
            }

            mapping(found) = let
          }

          p.const(c"|")()

          def digit(word: CString): Int = {
            var result = -1
            loops.breakable(0, 9, stopWhen = result != -1) { idx =>
              if (blueprint(idx) == toBin(word))
                result = idx
            }
            result
          }

          var acc = 0

          while (string.strcmp(p.remainingString, c"\n") != 0) {
            val word = stackalloc[CChar](7)
            p.const(c" ")().string(word)

            val len = string.strlen(word).toInt
            loops.loop(0, len - 1) { idx =>
              val originalOffset = word(idx) - 'a'
              val translatedChar = 'a' + mapping(originalOffset)
              word(idx) = translatedChar.toByte
            }
            acc = acc * 10 + digit(word)
          }

          println(acc)

          answer += acc
        }

        answer
      }

      println(s"Part 1: ${part_1_answer}")
      println(s"Part 2: ${part_2_answer}")
    }
  }
}
