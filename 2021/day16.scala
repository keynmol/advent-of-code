// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.libc.stdio
import scala.scalanative.unsafe._

object Day16 extends SyntaxHelpers {

  def resolve(hex: CChar) = hex match {
    case '0' => c"0000"
    case '1' => c"0001"
    case '2' => c"0010"
    case '3' => c"0011"
    case '4' => c"0100"
    case '5' => c"0101"
    case '6' => c"0110"
    case '7' => c"0111"
    case '8' => c"1000"
    case '9' => c"1001"
    case 'A' => c"1010"
    case 'B' => c"1011"
    case 'C' => c"1100"
    case 'D' => c"1101"
    case 'E' => c"1110"
    case 'F' => c"1111"
  }

  val i = Iterator.from(1)
  val READING_VERSION = i.next()
  val READING_TYPE = i.next()
  val READING_CONTENTS = i.next()

  class BitProducer(line: CString) {
    val len = strings.len(line) - 1
    def get(i: Int): Int = {
      val hexLetter = i / 4
      val offset = i % 4

      resolve(line(hexLetter))(offset) - '0'
    }

    def maxIndex = len * 4 - 1
  }

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      // val red = toCString(Console.RED)
      // val yello = toCString(Console.YELLOW)
      // val cyan = toCString(Console.CYAN)
      // val green = toCString(Console.GREEN)
      // val reset = toCString(Console.RESET)

      val packetTypes = WrappedArray.create[Int]()
      files.lines(args.head) { line =>
        val producer = new BitProducer(line)
        var part_1_answer = 0
        val part_2_stack = Stack.create[Long]
        var packet_depth = 0

        def readProtocolId(from: Int): Int = {
          var protocol_version = 0
          loops.loop(from, from + 2) { idx =>
            protocol_version += producer.get(idx) * 1 << (2 - (idx - from))
          }

          print_pointer(from + 3, s"Protocol: $protocol_version", 3)
          part_1_answer += protocol_version

          from + 3
        }
        def readTypeId(from: Int): Int = {
          var typeId = 0
          loops.loop(from, from + 2) { idx =>
            typeId += producer.get(idx) * 1 << (2 - (idx - from))
          }
          packetTypes.appendAndGrow(typeId)
          print_pointer(from + 3, s"Type: $typeId", 3)

          from + 3
        }

        def readBits(from: Int): Int = {
          var number = 0L

          def readBit(start: Int) = {
            val first = producer.get(start)
            number = number << 4
            loops.loop(start + 1, start + 4) { i =>
              val bit = producer.get(i)
              number += bit * (1 << (3 - (i - (start + 1))))
            }

            first
          }

          var cnt = 1

          while ({
            val offset = from + (cnt - 1) * 5
            val result = readBit(offset)
            result != 0
          }) {
            cnt += 1
          }

          print_pointer(
            from + cnt * 5,
            s"consumed $cnt values and the decimal is $number",
            cnt * 5
          )
          part_2_stack.push(number)
          cnt * 5
        }

        def readNum(from: Int, length: Int) = {
          var num = 0
          loops.loop(from, from + length - 1) { i =>
            num += producer.get(i) * (1 << (from + length - 1 - i))
          }
          num
        }

        def evaluate(typ: Int, subpackets: Int) = {
          var expr = 0L
          typ match {
            case 0 =>
              loops.loop(1, subpackets) { _ =>
                expr += part_2_stack.pop()
              }
            case 1 =>
              expr = 1
              loops.loop(1, subpackets) { _ =>
                expr *= part_2_stack.pop()
              }
            case 2 =>
              var min = Long.MaxValue
              loops.loop(1, subpackets) { _ =>
                val value = part_2_stack.pop()
                if (value < min) min = value
              }
              expr = min
            case 3 =>
              var max = Long.MinValue
              loops.loop(1, subpackets) { _ =>
                val value = part_2_stack.pop()
                if (value > max) max = value
              }
              expr = max
            case 5 =>
              val second = part_2_stack.pop()
              val first = part_2_stack.pop()

              expr = if (first > second) 1 else 0
            case 6 =>
              val second = part_2_stack.pop()
              val first = part_2_stack.pop()

              expr = if (first < second) 1 else 0
            case 7 =>
              val second = part_2_stack.pop()
              val first = part_2_stack.pop()

              expr = if (first == second) 1 else 0

          }

          part_2_stack.push(expr)
        }

        def print_pointer(point: Int, msg: String, highlight: Int = 0) = {
          // def spacer(i: Int) = {
          //   if (i > 0 && i % 5 == 0) stdio.printf(c" ")
          // }
          // loops.loop(0, producer.maxIndex) { idx =>
          //   spacer(idx)
          //   stdio.printf(c"%1d", producer.get(idx))
          // }
          // stdio.printf(c"\n")
          // loops.loop(0, producer.maxIndex) { idx =>
          //   spacer(idx)
          //   if (point == idx) {
          //     stdio.printf(yello)
          //     stdio.printf(c"^", idx)
          //     stdio.printf(reset)
          //   } else if (idx >= point - highlight && idx < point) {
          //     stdio.printf(green)
          //     stdio.printf(c"'", idx)
          //     stdio.printf(reset)
          //   } else stdio.printf(c" ")

          // }
          // stdio.printf(c"\n")

          // stdio.printf(cyan)
          // loops.loop(1, packet_depth) { _ =>
          //   stdio.printf(c" > ")
          // }
          // stdio.printf(toCString(msg))
          // stdio.printf(reset)
          // stdio.printf(c"\n")
          // stdio.printf(red)
          // loops.loop(0, producer.maxIndex) { idx =>
          //   stdio.printf(c"-", idx)

          // }
          // stdio.printf(reset)
          // stdio.printf(c"\n")
        }

        def readPacket(from: Int): Int = {
          packet_depth += 1
          val f = readProtocolId _ andThen readTypeId _

          val next = f(from)

          val typeId = !packetTypes.at(packetTypes.size - 1)
          val totalConsumed = if (typeId == 4) {
            val consumed = readBits(next)
            val totalBits = consumed + 6
            print_pointer(
              next + consumed,
              s"Literal value packed total consumed: ${totalBits}"
            )
            totalBits
          } else {
            val lengthId = producer.get(next)
            var subpackets = 0
            val totalConsumed = if (lengthId == 0) {
              val bits = readNum(next + 1, 15)
              val offset = next + 1 + 15
              print_pointer(
                offset,
                s"length id is 0, so skipped 15 bits (read $bits number)",
                15
              )
              var consumed = 0

              while (consumed < bits) {
                print_pointer(
                  offset + consumed,
                  s"Reading packet (${bits - consumed} remaining)"
                )
                consumed += readPacket(offset + consumed)
                print_pointer(
                  offset + consumed,
                  s"Consumed $consumed so far...",
                  consumed
                )
                subpackets += 1
              }

              6 + 16 + consumed
            } else {
              val numPackets = readNum(next + 1, 11)
              val startingPoint = next + 1 + 11
              var consumed = 0
              print_pointer(
                startingPoint,
                s"Length id was 1, so read 11 bits and came up with number $numPackets",
                11
              )

              while (subpackets < numPackets) {
                print_pointer(
                  consumed + startingPoint,
                  s"Reading packet ${subpackets + 1} out of $numPackets",
                  consumed
                )
                consumed += readPacket(consumed + startingPoint)

                print_pointer(
                  consumed + startingPoint,
                  s"Consumed $consumed so far",
                  consumed
                )
                subpackets += 1
              }

              6 + 12 + consumed
            }

            print_pointer(
              next + totalConsumed,
              s"Consumed $subpackets sub packets"
            )
            evaluate(typeId, subpackets)
            totalConsumed

          }

          packet_depth -= 1
          totalConsumed
        }

        readPacket(0)

        stdio.printf(c"Part 1: %d\n", part_1_answer)
        stdio.printf(c"Part 2: %d\n", part_2_stack.pop())

      }

    }
  }
}
