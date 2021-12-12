// using scala 3.0.2
// using lib com.lihaoyi::os-lib:0.7.8
// using lib com.eed3si9n.expecty::expecty:0.15.4

import scala.util.Try
import com.eed3si9n.expecty.Expecty.{assert as expect}

case class A(part1: Long, part2: Long):
  def render = s"Part 1: $part1\nPart 2: $part2"

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
    12 -> A(10, 36)
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

  val part2 = lines.collectFirst { case s"Part 2: $num" =>
    num.toLong
  }.get

  val answer = A(part1, part2)

  expect(answers(year)(day) == answer)
end hello
