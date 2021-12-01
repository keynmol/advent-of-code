// using scala 2.13.6
// using scala-native
// using lib org.scalameta::munit::0.7.27

import scalanative.libc.stdio
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.libc.stdlib
import scala.scalanative.annotation.alwaysinline
import scala.scalanative.libc

final case class WrappedArray[T: Tag] private (
    var chunksData: Ptr[Ptr[T]],
    private val chunkSize: Int,
    private var used: Int
) {
  @inline private def usedChunks = {
    if (used < chunkSize) 1
    else {
      val total = used / chunkSize
      val rem = used % chunkSize
      if (rem > 0) total + 1 else total
    }
  }

  def foreach(f: T => Unit) {
    var i = 0
    while (i < used) {
      f(!at(i))
      i += 1
    }
  }

  def foreachBreakable(break: => Boolean)(f: T => Unit) {
    var i = 0
    while (!break && i < used) {
      f(!at(i))
      i += 1
    }
  }

  def exists(cond: T => Boolean): Boolean = {
    var found = false
    foreachBreakable(found) { t =>
      found = cond(t)
    }

    found
  }

  def foreachPtr(f: Ptr[T] => Unit) {
    var i = 0
    while (i < used) {
      f(at(i))
      i += 1
    }
  }

  @alwaysinline def size = used

  @inline private def maxCapacity = {
    usedChunks * chunkSize
  }

  @alwaysinline def at(n: Int): Ptr[T] = {
    val chunkLocation = (n / chunkSize).toUInt
    val dataLocation = (n % chunkSize).toUInt
    val chunkOffset = chunkLocation * sizeof[Ptr[T]]
    val chunkPtr = !(chunksData + chunkLocation)
    val dataOffset = sizeof[T] * dataLocation
    (chunkPtr + dataLocation.toUInt)
  }

  @alwaysinline def addChunk()(implicit z: Zone) = {
    val newChunksData = alloc[Ptr[T]](usedChunks + 1)
    // copy
    var i = 0
    while (i < usedChunks) {
      !(newChunksData + i) = !(chunksData + i)

      i += 1
    }

    val newChunk = alloc[T](chunkSize)
    !(newChunksData + i) = newChunk
    this.chunksData = newChunksData
  }

  @alwaysinline private def justAppend(value: T) = {
    !(at(used)) = value
    this.used += 1
  }

  def appendAndGrow(value: T)(implicit z: Zone): Unit = {
    if (used < maxCapacity) {
      justAppend(value)
    } else {
      addChunk()
      justAppend(value)
    }
  }
}

object WrappedArray {
  def create[T: Tag](chunkSize: Int = 100)(implicit z: Zone) = {
    val singleChunk = alloc[T](chunkSize)
    val data = alloc[Ptr[T]](1)

    !(data) = singleChunk
    WrappedArray(data, chunkSize, 0)
  }
}

object strings {
  @inline def foreachChar(string: Ptr[CChar])(f: CChar => Unit) = {
    var i = 0
    var char: CChar = -1;
    while ({ char = string(i); char } != 0) {
      f(char)
      i += 1
    }
  }

  def foreachCharWithIndex(string: Ptr[CChar])(f: (CChar, Int) => Unit) = {
    var i = 0
    var char: CChar = -1;
    while ({ char = string(i); char } != 0) {
      f(char, i)
      i += 1
    }
  }
}

object loops {
  @alwaysinline def loop(from: Int, to: Int, inclusive: Boolean = true)(
      f: Int => Unit
  ) {
    var i = from
    while (i <= { if (inclusive) to else to - 1 }) {
      f(i)
      i += 1
    }
  }
}

object files {
  def lines(filename: String)(f: CString => Unit)(implicit z: Zone) = {
    linesWithIndex(filename)((s, _) => f(s))
  }
  def linesWithIndex(filename: String, maxLineLength: Int = 100)(
      f: (CString, Int) => Unit
  )(implicit z: Zone) = {
    val file = stdio.fopen(toCString(filename), c"r")
    val line = alloc[CChar](maxLineLength)
    var i = 0
    while (stdio.fgets(line, maxLineLength, file) != null) {
      f(line, i)
      i += 1
    }

    stdio.fclose(file)
  }
}

object parser {
  type Remainder = CString
  type Consumed = UInt
  type Length = UInt
  type Cursor = CStruct3[Consumed, Remainder, Length]

  type Parser1[A] = Ptr[A] => Parser
  type Parser0[A] = Function0[Parser]

  def init(str: CString)(implicit z: Zone): Parser = {

    val cur = alloc[CStruct3[Consumed, Remainder, Length]]
    !(cur.at1) = 0.toUInt
    !(cur.at2) = str
    !(cur.at3) = libc.string.strlen(str).toUInt

    new Parser(!cur)
  }

  class Parser private[parser] (cursor: Cursor)(implicit z: Zone) {

    final private val ONE = 1.toUInt
    final private var success = true

    @alwaysinline private def shift(n: UInt): Parser = {
      if (success) {
        cursor._1 = cursor._1 + n
        cursor._2 = cursor._2 + n
      }
      this
    }

    @alwaysinline val string: Parser1[CChar] = ptr => {
      val n = stackalloc[UInt]
      success = stdio.sscanf(cursor._2, c"%s%n", ptr, n) == 1

      shift(!n)
    }

    @alwaysinline val long: Parser1[Long] = ptr => {
      val n = stackalloc[UInt]
      success = stdio.sscanf(cursor._2, c"%ld%n", ptr, n) == 1

      shift(!n)
    }
    @alwaysinline val int: Parser1[Int] = ptr => {
      val n = stackalloc[UInt]
      success = stdio.sscanf(cursor._2, c"%d%n", ptr, n) == 1

      shift(!n)
    }

    @alwaysinline val space: Parser0[Nothing] = () => {
      stdio.sscanf(cursor._2, c" ")
      shift(ONE)
    }
    @alwaysinline val newline: Parser0[Nothing] = () => {
      stdio.sscanf(cursor._2, c"\n")
      shift(ONE)
    }

    def const(pattern: CString): Parser0[Nothing] = () => {
      val len = libc.string.strlen(pattern)
      success = libc.string.strncmp(cursor._2, pattern, len) == 0

      shift(len.toUInt)
      this
    }

    @alwaysinline val char: Parser1[CChar] = ptr => {
      success = stdio.sscanf(cursor._2, c"%c", ptr) == 1

      shift(ONE)
    }

    def remainingLength: UInt = cursor._3 - cursor._1
    def remainingString: CString = cursor._2
    def consumed: UInt = cursor._1
    def finished: Boolean = cursor._1 == cursor._3

    @alwaysinline val rewind: Parser0[Nothing] = () => {
      cursor._2 = cursor._2 - cursor._1
      cursor._1 = 0.toUInt

      this
    }
  }

}

class ParseTests extends munit.FunSuite {
  test("simple parsing and rewinding") {
    Zone { implicit z =>
      val STR = c"hello 25. //--> 1112312312312\n"

      val cur = parser.init(STR)
      val key = stackalloc[CChar](100)
      val value = stackalloc[Int]
      val commentNum = stackalloc[Long]
      val char = stackalloc[CChar]

      cur
        .string(key)
        .space()
        .int(value)
        .char(char)
        .space()
        .const(c"//-->")()
        .space()
        .long(commentNum)
        .newline()

      assert(!value == 25)
      assertEquals(fromCString(key), "hello")
      assertEquals(!char, '.'.toByte)
      assertEquals(!commentNum, 1112312312312L)
      assertEquals(cur.remainingLength.toInt, 0)
      assert(cur.finished)

      cur.rewind()

      assertEquals(cur.remainingLength, libc.string.strlen(STR).toUInt)
      assertEquals(libc.string.strcmp(cur.remainingString, STR).toInt, 0)
      assertEquals(cur.consumed, 0.toUInt)
    }
  }
}
