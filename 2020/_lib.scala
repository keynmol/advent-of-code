// using scala 2.13.6
// using scala-native

import scalanative.libc.stdio
import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scala.scalanative.libc.stdlib
import scala.scalanative.annotation.alwaysinline

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
