// using scala 2.13.6
// using scala-native
// using lib org.scalameta::munit::0.7.27

import scalanative.libc.stdio
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.annotation.alwaysinline
import scala.scalanative.libc
import scala.util.chaining._

trait ToPrintF[T] {
  def format: CString
}

object ToPrintF {
  def as[T](cstr: CString) = new ToPrintF[T] {
    def format = cstr
  }
  implicit val intPrintF: ToPrintF[Int] = as[Int](c"%d")
  implicit val strPrintF: ToPrintF[CString] = as[CString](c"%s")
}

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

  def foreach = new PartiallyAppliedForeach[T](this)

  def exists(cond: T => Boolean): Boolean = {
    var found = false
    foreach.breakable(found) { t =>
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

  private def location(n: Int): Ptr[T] = {
    val chunkLocation = (n / chunkSize).toUInt
    val dataLocation = (n % chunkSize).toUInt
    // val chunkOffset = chunkLocation * sizeof[Ptr[T]]
    val chunkPtr = !(chunksData + chunkLocation)
    // val dataOffset = sizeof[T] * dataLocation
    (chunkPtr + dataLocation.toUInt)
  }

  @alwaysinline def at(n: Int): Ptr[T] = {
    location(n)
    // val chunkLocation = (n / chunkSize).toUInt
    // val dataLocation = (n % chunkSize).toUInt
    // // val chunkOffset = chunkLocation * sizeof[Ptr[T]]
    // val chunkPtr = !(chunksData + chunkLocation)
    // val dataOffset = sizeof[T] * dataLocation
    // (chunkPtr + dataLocation.toUInt)
  }

  def set(idx: Int, value: T) = {
    !location(idx) = value
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

  def indexOf(t: T) = {
    var found = -1
    foreach.breakableWithIndex(stopWhen = found != -1) { case (el, idx) =>
      if (el == t) found = idx

    }
    found
  }

  class PartiallyAppliedForeach[T: Tag](ar: WrappedArray[T]) {
    @alwaysinline def apply(f: T => Unit) = {
      withIndex((el, _) => f(el))
    }

    def withIndex(f: (T, Int) => Unit) = {
      var i = 0
      while (i < used) {
        f(!ar.at(i), i)
        i += 1
      }
    }
    def breakable(stopWhen: => Boolean)(f: T => Unit) = {
      breakableWithIndex(stopWhen)((el, _) => f(el))
    }
    def breakableWithIndex(stopWhen: => Boolean)(f: (T, Int) => Unit) = {
      var i = 0
      while (!stopWhen && i < used) {
        f(!ar.at(i), i)
        i += 1
      }
    }
  }

  def continuous(mem: Ptr[T]) = {
    foreach.withIndex { case (t, idx) =>
      !(mem + idx) = t
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
object Matrix {
  type Typ[T] = CStruct3[Ptr[T], Int, Int]

  def create[T: Tag](width: Int, height: Int)(implicit
      z: Zone
  ): Matrix.Typ[T] = {
    val data = alloc[T](width * height)
    val mt = alloc[Matrix.Typ[T]](1)
    mt._1 = data
    mt._2 = width
    mt._3 = height

    mt

  }
  implicit class Mops[T: Tag](val ptr: Typ[T]) {
    final def width = ptr._2
    final def height = ptr._3
    final def data = ptr._1

    @alwaysinline final def pack(row: Int, col: Int) = {
      val default = -1
      if (row >= height) default
      else if (col >= width) default
      else if (row < 0) default
      else if (col < 0) default
      else row * width + col
    }

    @alwaysinline final def unpackRow(dense: Int) = {
      dense / width
    }
    @alwaysinline final def unpackCol(dense: Int) = {
      dense % width
    }

    @alwaysinline final def maxRow = height - 1
    @alwaysinline final def maxCol = width - 1
    @alwaysinline final def at(row: Int, col: Int, default: T): T = {
      if (row >= height) default
      else if (col >= width) default
      else if (row < 0) default
      else if (col < 0) default
      else data(row * width + col)
    }

    @alwaysinline final def unsafe(row: Int, col: Int): T = {
      data(row * width + col)
    }

    @alwaysinline final def valid(row: Int, col: Int): Boolean = {
      if (row >= height) false
      else if (col >= width) false
      else if (row < 0) false
      else if (col < 0) false
      else true
    }

    @alwaysinline final def set(row: Int, col: Int, value: T): Boolean = {
      if (valid(row, col)) {
        data(row * width + col) = value
        true
      } else false
    }

    @alwaysinline final def left(row: Int, col: Int, default: T): T = {
      if (col == 0) default
      else at(row, col - 1, default)
    }
    @alwaysinline final def right(row: Int, col: Int, default: T): T = {
      if (col == width - 1) default
      else at(row, col + 1, default)
    }
    @alwaysinline final def down(row: Int, col: Int, default: T): T = {
      if (row == height - 1) default
      else at(row + 1, col, default)
    }
    @alwaysinline final def up(row: Int, col: Int, default: T): T = {
      if (row == 0) default
      else at(row - 1, col, default)
    }

    final def copy(implicit z: Zone): Matrix.Typ[T] = {
      val space = alloc[T](width * height)
      val newMt = alloc[Matrix.Typ[T]](1)
      newMt._1 = space
      newMt._2 = width
      newMt._3 = height
      loops.loop(0, maxRow) { row =>
        loops.loop(0, maxCol) { col =>
          space(row * width + col) = ptr._1.apply(row * width + col)
        }
      }
      newMt
    }

    final def reset(value: T): Matrix.Typ[T] = {
      loops.loop(0, maxRow) { row =>
        loops.loop(0, maxCol) { col =>
          set(row, col, value)
        }
      }
      ptr
    }

    final def printInt(
        format: CString,
        highlightValues: Bitset.Typ
    )(implicit z: Zone, a: T =:= Int) = {
      import Bitset._
      val red = toCString(Console.RED)
      val yello = toCString(Console.YELLOW)
      val green = toCString(Console.CYAN + Console.BOLD)
      val reset = toCString(Console.RESET)
      stdio.printf(yello)
      stdio.printf(c"    ")
      loops.loop(0, maxCol) { col =>
        stdio.printf(c"%6d", col)
      }
      stdio.printf(reset)
      stdio.printf(c"\n")

      loops.loop(0, maxRow) { row =>
        stdio.printf(yello)
        stdio.printf(format, row)
        stdio.printf(reset)
        loops.loop(0, maxCol) { col =>
          if (highlightValues.get(unsafe(row, col)))
            stdio.printf(green)
          stdio.printf(format, unsafe(row, col))
          stdio.printf(reset)

        }
        stdio.printf(c"\n")
      }
    }

    final def print(
        format: CString,
        highlight: (Int, Int, T) => Boolean = (_, _, _) => false
    )(implicit z: Zone) = {
      import Bitset._
      val red = toCString(Console.RED)
      val yello = toCString(Console.YELLOW)
      val green = toCString(Console.GREEN)
      val reset = toCString(Console.RESET)
      stdio.printf(yello)
      stdio.printf(c"    ")
      loops.loop(0, maxCol) { col =>
        stdio.printf(format, col)
      }
      stdio.printf(reset)
      stdio.printf(c"\n")

      loops.loop(0, maxRow) { row =>
        stdio.printf(yello)
        stdio.printf(format, row)
        stdio.printf(reset)
        loops.loop(0, maxCol) { col =>
          if (highlight(row, col, unsafe(row, col)))
            stdio.printf(green)
          stdio.printf(format, unsafe(row, col))
          stdio.printf(reset)

        }
        stdio.printf(c"\n")
      }
    }
  }
}
class Stack[T: Tag] private (internal: WrappedArray[T]) {
  private var current = -1
  def isEmpty: Boolean = current == -1
  def reset() =
    current = -1 // note that we don't free up the memory :(
  def push(t: T)(implicit z: Zone) = {
    val lastIdx = internal.size - 1
    current += 1
    if (current > lastIdx) internal.appendAndGrow(t)
    else {
      !internal.at(current) = t
    }
    internal.appendAndGrow(t)
  }

  def pop() = {
    !(internal.at(current)).tap { _ => current -= 1 }
  }

  def peek(): T = {
    !(internal.at(current))
  }
}

object Stack {
  def create[T: Tag](implicit z: Zone) = {
    val int = WrappedArray.create[T]()

    new Stack(int)
  }
}

object SlowIntMap {
  type Typ = WrappedArray[Long]
  def create(implicit z: Zone): Typ = {
    WrappedArray.create[Long]()
  }

  @alwaysinline private def pack(row: Int, col: Int) = {
    (row.toLong << 32) + col.toLong
  }

  @alwaysinline private def unpackFirst(dense: Long) = {
    (dense >>> 32).toInt
  }

  @alwaysinline private def unpackSecond(dense: Long) = {
    ((dense << 32) >>> 32).toInt
  }

  implicit class Ops(im: SlowIntMap.Typ) {
    private def indexOf(key: Int) = {
      var idx = -1
      loops.breakable(0, im.size - 1, stopWhen = idx != -1) { i =>
        val unpackedKey = unpackFirst(!im.at(i))
        // stdio.printf(c"%d == %d\n", unpackedKey, key)
        if (unpackedKey == key)
          idx = i
      }

      idx
    }
    def put(key: Int, value: Int)(implicit z: Zone) = {
      val packed = pack(key, value)
      val idx = indexOf(key)
      if (idx == -1)
        im.appendAndGrow(packed)
      else
        im.set(idx, packed)
    }

    def getOrElse(key: Int, default: Int): Int = {
      val idx = indexOf(key)
      if (idx == -1)
        default
      else
        unpackSecond(!(im.at(idx)))
    }
  }
}

object Bitset {
  // first 16 bits are the number of Longs
  type Typ = Ptr[Int]
  private val bits = sizeof[Int].toInt * 8
  @alwaysinline def size(typ: Typ) = {
    val numLongs = typ(0)
    numLongs
  }

  @alwaysinline private def blocks(elements: Int) = {
    if ((elements % bits) > 0) 1 + (elements / bits) else elements / bits
  }
  def create(elements: Int)(implicit z: Zone): Typ = {
    // val nBlocks =
    //   if ((elements % bits) > 0) 1 + (elements / bits) else elements / bits

    val memory = alloc[Int](1 + blocks(elements))
    memory(0) = elements

    memory
  }

  implicit class typOps(val t: Typ) {
    @inline def set(n: Int) = {
      // val sz = size(t)
      // if (n <= sz) {
      val blockId = 1 + (n / bits)
      val bitId = n % bits
      val mask = 1 << (bits - bitId - 1)
      t(blockId) = t(blockId) | mask
      // }
    }
    @inline def unset(n: Int) = {
      // val sz = size(t)
      // if (n <= sz) {
      val blockId = 1 + (n / bits)
      val bitId = n % bits
      val mask = 1 << (bits - bitId - 1)
      t(blockId) = t(blockId) & ~mask
      // }
    }

    def foreach(el: Int => Unit) = {
      var blockId = 1
      while (blockId <= blocks(size(t))) {
        val block = t(blockId)
        if (block != 0) {
          val blockOffset = (blockId - 1) * bits
          loops.loop(0, bits - 1) { bitId =>
            val mask = 1 << (bits - bitId - 1)
            if ((block & mask) == mask) {
              el(blockOffset + bitId)
            }

          }
        }

        blockId += 1
      }

    }

    @inline def get(n: Int): Boolean = {
      // val sz = size(t)
      // if (n <= sz) {
      val blockId = 1 + (n / bits)
      val bitId = n % bits
      val mask = 1 << (bits - bitId - 1)
      (t(blockId) & mask) == mask
      // } else false
    }

    def empty: Boolean = {
      var nonEmpty = false
      var i = 1
      while (!nonEmpty && (i <= blocks(size(t)))) {
        nonEmpty = t(i) != 0
        i += 1
      }

      !nonEmpty
    }

    def str = {
      stdio.printf(c"[Bitset (max %d elements): ", size(t))
      loops.loop(1, size(t)) { i =>
        if (get(i)) stdio.printf(c"%d, ", i)
      }
      stdio.printf(c"]\n")
    }

    def copy(implicit z: Zone): Typ = {
      val newMem = alloc[Int](blocks(size(t)) + 1)
      newMem(0) = size(t)
      loops.loop(1, blocks(size(t))) { i =>
        newMem(i) = t(i)
      }

      newMem
    }
  }
}

object strings {
  @alwaysinline def len(str: CString) = libc.string.strlen(str).toInt

  @alwaysinline def foreachChar(string: Ptr[CChar])(f: CChar => Unit) = {
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

  def contains(str: CString, char: CChar, len: Int) = {
    var found = false
    loops.breakable(0, len - 1, stopWhen = found) { idx =>
      found = str(idx) == char
    }

    found
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
  @alwaysinline def breakable(
      from: Int,
      to: Int,
      stopWhen: => Boolean,
      inclusive: Boolean = true
  )(
      f: Int => Unit
  ) {
    var i = from
    while (!stopWhen && i <= { if (inclusive) to else to - 1 }) {
      f(i)
      i += 1
    }
  }
}

object files {
  def lines(filename: String)(f: CString => Unit)(implicit z: Zone) = {
    linesWithIndex(filename)((s, _) => f(s))
  }
  def linesWithIndex(filename: String, maxLineLength: Int = 10_000)(
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

  def parsedLines(filename: String)(f: (CString, parser.Parser) => Unit)(
      implicit z: Zone
  ) = { parsedLinesWithIndex(filename)((s, p, _) => f(s, p)) }

  def parsedLinesWithIndex(filename: String, maxLineLength: Int = 100)(
      f: (CString, parser.Parser, Int) => Unit
  )(implicit z: Zone) = {
    val file = stdio.fopen(toCString(filename), c"r")
    val line = alloc[CChar](maxLineLength)
    var i = 0
    val p = parser.init(line)
    while (stdio.fgets(line, maxLineLength, file) != null) {
      f(line, p, i)
      i += 1
      p.rewind()
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

  class Parser private[parser] (private var cursor: Cursor) {

    final private val ONE = 1.toUInt
    final private var success = true

    def resetInPlace(str: CString)(implicit z: Zone) = {
      cursor = alloc[CStruct3[Consumed, Remainder, Length]]
      !(cursor.at1) = 0.toUInt
      !(cursor.at2) = str
      !(cursor.at3) = libc.string.strlen(str).toUInt

      this
    }

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

    // @alwaysinline val char: Parser1[Byte] = ptr => {
    //   val n = stackalloc[Byte]
    //   success = stdio.sscanf(cursor._2, c"%c%n", ptr, n) == 1

    //   shift(!n.toUInt)
    // }

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
    def successful = success

    @alwaysinline val rewind: Parser0[Nothing] = () => {
      cursor._2 = cursor._2 - cursor._1
      cursor._1 = 0.toUInt

      this
    }
  }

}

trait SyntaxHelpers {
  implicit class StringOps(i: Int) {
    def bin(n: Int) = {
      i.toBinaryString.reverse.padTo(n, '0').reverse
    }
  }
}

object MathShit {
  type Coordinate = CStruct3[Int, Int, Int]
  implicit final class Cops(val co: Coordinate) extends AnyVal {
    @alwaysinline def x = co._1
    @alwaysinline def y = co._2
    @alwaysinline def z = co._3
    @alwaysinline def setX(value: Int) =
      co._1 = value
    @alwaysinline def setY(value: Int) =
      co._2 = value
    @alwaysinline def setZ(value: Int) =
      co._3 = value

    @alwaysinline def copy(implicit zone: Zone): Coordinate = {
      Coordinate.create(x, y, z)
    }

    // @alwaysinline def manhattan(other: Coordinate): Int = {
    //   import scala.scalanative.libc.math.abs
    //   abs(x - other.x) +
    //     abs(y - other.y) +
    //     abs(z - other.z)
    // }

    @alwaysinline def euclid(other: Coordinate): Int = {
      @alwaysinline def sq(i: Int) = i * i
      sq(x - other.x) + sq(y - other.y) + sq(z - other.z)
    }

    @alwaysinline def ===(other: Coordinate) =
      x == other.x && y == other.y && z == other.z

    def show: String = {
      s"Coordinate($x, $y, $z)"
    }

    def isZero =
      x == 0 && y == 0 && z == 0

    def negate: Coordinate = {
      setX(-x)
      setY(-y)
      setZ(-z)
      co
    }

  }
  object Coordinate {
    def create(x: Int, y: Int, z: Int)(implicit zone: Zone): Coordinate = {
      val mem = alloc[Coordinate](1)
      mem._1 = x
      mem._2 = y
      mem._3 = z

      mem
    }

  }

  type RotationMatrix = CStruct3[Int, Int, Int]
  implicit class Rops(rm: RotationMatrix) {
    def rotate(coord: Coordinate) = {
      if (rm._1 != 0) {
        val xCosine = IntegerCosine(rm._1)
        val xSine = IntegerSine(rm._1)

        val x = coord.x
        val y = coord.y
        val z = coord.z

        coord.setX(x)
        coord.setY(y * xCosine - z * xSine)
        coord.setZ(y * xSine + z * xCosine)
      }

      if (rm._2 != 0) {
        val x = coord.x
        val y = coord.y
        val z = coord.z
        val yCosine = IntegerCosine(rm._2)
        val ySine = IntegerSine(rm._2)
        // println(s"howdy cos=$yCosine sin=$ySine")

        coord.setX(z * ySine + x * yCosine)
        coord.setY(y)
        coord.setZ(z * yCosine - x * ySine)
      }

      if (rm._3 != 0) {
        val x = coord.x
        val y = coord.y
        val z = coord.z
        val zCosine = IntegerCosine(rm._3)
        val zSine = IntegerSine(rm._3)

        coord.setX(x * zCosine - y * zSine)
        coord.setY(x * zSine + y * zCosine)
        coord.setZ(z)
      }

      coord

    }

    def set(x: Int, y: Int, z: Int) = {
      rm._1 = x
      rm._2 = y
      rm._3 = z
    }

  }
  object RotationMatrix {
    def create(turnsAlongX: Int, turnsAlongY: Int, turnsAlongZ: Int)(implicit
        z: Zone
    ): RotationMatrix = {
      val m = alloc[RotationMatrix](1)

      m._1 = turnsAlongX
      m._2 = turnsAlongY
      m._3 = turnsAlongZ

      m
    }
  }

  def IntegerCosine(turns: Int): Int =
    if (turns % 4 == 0) 1 else if (turns % 2 == 0) -1 else 0

  def IntegerSine(turns: Int): Int =
    if (turns < 0) -IntegerSine(-turns)
    else {
      if (turns % 2 == 1) 1 else if ((turns - 1) % 4 == 0) -1 else 0
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

class IntMapTests extends munit.FunSuite {
  test("IntMap acts like a map") {
    Zone { implicit z =>
      import SlowIntMap._
      val mp = SlowIntMap.create
      mp.put(25, 128)
      mp.put(0, 1024)
      mp.put(378, 873)
      mp.put(25, 128)

      assertEquals(mp.getOrElse(25, -1), 128)
      assertEquals(mp.getOrElse(378, -1), 873)
      assertEquals(mp.getOrElse(0, -1), 1024)
      assertEquals(mp.getOrElse(1, -1), -1)
      assertEquals(mp.getOrElse(24, -1), -1)
    }
  }
}

class BitSetTests extends munit.FunSuite {

  test("Bitset.foreach works for a full set") {
    Zone { implicit z =>
      val M = 132
      val bs = Bitset.create(M)
      import Bitset._

      (1 to M).foreach { i =>
        bs.set(i)
      }

      val ls = List.newBuilder[Int]

      bs.foreach { el =>
        ls.addOne(el)
      }

      assertEquals(ls.result(), (1 to M).toList)
    }
  }

  test("Bitset.foreach works for a incomplete set") {
    Zone { implicit z =>
      val M = 132
      val bs = Bitset.create(M)
      import Bitset._

      (1 to M).filter(_ % 2 == 0).foreach { i =>
        bs.set(i)
      }

      val ls = List.newBuilder[Int]

      bs.foreach { el =>
        ls.addOne(el)
      }

      assertEquals(ls.result(), (1 to M).filter(_ % 2 == 0).toList)
    }
  }
}

class MathShitTests extends munit.FunSuite {
  import MathShit._
  test("I've no idea what I'm doing") {
    loops.loop(-2, 2) { i =>
      val sine = MathShit.IntegerSine(i)
      val cosine = MathShit.IntegerCosine(i)

      assertEquals(
        sine * sine + cosine * cosine,
        1,
        s"That trigonometry stuff holds for $i: sine is $sine, cosine is $cosine"
      )
    }
  }
  test("Sine is well-defined") {
    loops.loop(-2, 2) { i =>
      val sine = MathShit.IntegerSine(i)
      val rem = math.abs(i) % 2

      assertEquals(
        math.abs(sine) % 2,
        rem,
        s"That trigonometry stuff holds for $i: sine is $sine"
      )
    }
  }
  test("Cosine is well-defined") {
    loops.loop(-2, 2) { i =>
      val cosine = MathShit.IntegerCosine(i)
      val rem = math.abs(i) % 2

      assertEquals(
        math.abs(cosine) % 2,
        1 - rem,
        s"That trigonometry stuff holds for $i: cosine is $cosine"
      )
    }
  }

  test("rotate around x correct") {
    Zone { implicit z =>
      val coord = Coordinate.create(1, 2, 3)
      val c = RotationMatrix.create(1, 0, 0).rotate(coord.copy)

      assertEquals(c.show, Coordinate.create(1, -3, 2).show)
    }
  }
  test("rotate around y is correct") {
    Zone { implicit z =>
      val coord = Coordinate.create(1, 2, 3)
      val c = RotationMatrix.create(0, 1, 0).rotate(coord.copy)

      assertEquals(c.show, Coordinate.create(3, 2, -1).show)
    }
  }
  test("rotate around z is correct") {
    Zone { implicit z =>
      val coord = Coordinate.create(1, 2, 3)
      val c = RotationMatrix.create(0, 0, 1).rotate(coord.copy)

      assertEquals(c.show, Coordinate.create(-2, 1, 3).show)
    }
  }
  // test("rotate around y is reversible") {
  //   Zone { implicit z =>
  //     val coord = Coordinate.create(1, 2, 3)
  //     loops.loop(-100, 100) { x =>
  //       val rm = RotationMatrix.create(0, x, 0)

  //       val rotated = rm.rotate(coord)
  //       val reversed = rm.negate.rotate(rotated)

  //       assertEquals(coord.show, reversed.show)
  //     }
  //   }
  // }
  // test("rotate around z is reversible") {
  //   Zone { implicit z =>
  //     val coord = Coordinate.create(1, 2, 3)
  //     loops.loop(-100, 100) { x =>
  //       val rm = RotationMatrix.create(0, 0, x)

  //       val rotated = rm.rotate(coord.copy)
  //       val reversed = rm.negate.rotate(rotated)

  //       assertEquals(coord.show, reversed.show)
  //     }
  //   }
  // }
}
