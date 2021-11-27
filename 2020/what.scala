// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.string

object What {

  def main(args: Array[String]): Unit = {
    Zone.apply { implicit z =>
      val num = stackalloc[Int]
      val str = stackalloc[CChar](100)
      string.strcpy(str, c"1856 hello\n")
      stdio.sscanf(str, c"%d hello\n", num)
      stdio.printf(c"Parsed result: %d", !num)
    }
  }
}
