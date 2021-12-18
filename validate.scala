// using scala 3.0.2
// using lib com.lihaoyi::os-lib:0.7.8
// using lib com.eed3si9n.expecty::expecty:0.15.4

import scala.util.Try
import com.eed3si9n.expecty.Expecty.{assert as expect}

case class A(part1: Long, part2: Option[Long]):
  def render = s"Part 1: $part1\nPart 2: $part2"

object A:
  def apply(part1: Long) = new A(part1, None)
  def apply(part1: Long, part2: Long) = new A(part1, Some(part2))

val answers = Map(
  2021 -> Map(
    1 -> A(7, 5),
    2 -> A(150, 900),
    3 -> A(198, 230),
    4 -> A(4512, 1924),
    5 -> A(5, 12),
    6 -> A(5934, 26984457539L),
    8 -> A(26, 61229),
    9 -> A(15, 1134),
    10 -> A(26397, 288957),
    11 -> A(1656, 195),
    12 -> A(10, 36),
    13 -> A(17),
    14 -> A(1588, 2188189693529L),
    15 -> A(40, 315),
    16 -> A(31, 54),
    17 -> A(45, 112)
  )
)

@main def hello(output: String, year: Int, day: Int) =
  val path =
    Try(os.Path(output)).getOrElse(os.RelPath(output).resolveFrom(os.pwd))

  val contents = os.read(path)

  val lines = contents.linesIterator.toList.map(_.trim).filterNot(_.isEmpty)

  val part1 = lines.collectFirst { case s"Part 1: $num" =>
    num.toLong
  }.get

  val expected = answers(year)(day)

  val answer = 
    expected.part2 match
      case None    => A(part1)
      case Some(_) => 
        val part2 = lines.collectFirst { case s"Part 2: $num" =>
          num.toLong
        }.get
        A(part1, part2)

  expect(answers(year)(day) == answer)
end hello
