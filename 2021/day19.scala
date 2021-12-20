// using scala 2.13.6
// using scala-native
// using options -Ywarn-unused

import scala.scalanative.unsafe._

object Day19 {
  import MathShit._

  def distanceMatrix(
      coords: Ptr[Coordinate],
      size: Int
  )(implicit z: Zone) = {
    import Matrix._
    val mtr = Matrix.create[Int](size, size)
    loops.loop(0, size - 1) { r =>
      loops.loop(0, size - 1) { c =>
        val a = coords(r)
        val b = coords(c)
        mtr.set(r, c, a.euclid(b))
      }
    }

    mtr
  }

  def isGood(
      beaconId: Int,
      distances1: Matrix.Typ[Int],
      distances2: Matrix.Typ[Int]
  ): Int = {
    import Matrix._

    var foundMatch = -1

    loops.breakable(0, distances2.maxRow, stopWhen = foundMatch != -1) {
      candidateBeaconId =>
        var matches = 0

        loops.loop(0, distances2.maxRow) { otherBeaconId =>
          val distance =
            distances2.unsafe(candidateBeaconId, otherBeaconId)

          var found = false
          loops.breakable(0, distances1.maxCol, stopWhen = found) { refId =>
            found = found || distances1.unsafe(beaconId, refId) == distance
          }

          if (found) matches += 1
        }

        // println(
        //   s"Beacon $beaconId from scanner 1 matches beacon $candidateBeaconId from scanner 2 with score $matches"
        // )

        if (matches >= 11) foundMatch = candidateBeaconId
    }

    foundMatch
  }
  def detect(
      sid1: Int,
      sid2: Int,
      scanners: Ptr[Coordinate],
      offsets: WrappedArray[Int],
      sizes: WrappedArray[Int],
      distancesAndRotations: Matrix.Typ[CStruct2[Coordinate, RotationMatrix]]
  )(implicit z: Zone) = {

    val scanner1 = scanners + !offsets.at(sid1)
    val scanner2 = scanners + !offsets.at(sid2)

    val distances1 = distanceMatrix(scanner1, !sizes.at(sid1))
    val distances2 = distanceMatrix(scanner2, !sizes.at(sid2))

    // distances1.print(c"%10d", { case (_, _, el) => el == 35429 })
    // distances2.print(c"%10d", { case (_, _, el) => el == 35429 })

    val mapping = stackalloc[Int](!sizes.at(sid1))

    var totalOverlapping = 0
    loops.loop(0, !sizes.at(sid1) - 1) { beaconId =>
      val matching = isGood(beaconId, distances1, distances2)
      mapping(beaconId) = matching
      if (matching != -1)
        totalOverlapping += 1
    }

    println(s"Overlapping $totalOverlapping")

    if (totalOverlapping == 12) {
      // loops.loop(0, !sizes.at(sid1) - 1) { beaconId =>
      //   val matchingBeaconId = mapping(beaconId)
      //   if (matchingBeaconId != -1) {
      //     println(
      //       s"""
      //       Scanner $sid1 sees ${scanner1(beaconId).show},
      //       scanner $sid2 sees ${scanner2(mapping(beaconId)).show}"""
      //     )
      //   }
      // }

      val rm = RotationMatrix.create(0, 0, 0)
      var offset: Coordinate = null

      loops.breakable(-2, 2, offset != null) { xO =>
        loops.breakable(-2, 2, offset != null) { yO =>
          loops.breakable(-2, 2, offset != null) { zO =>
            rm.set(xO, yO, zO)
            offset =
              detectOffset(mapping, scanner1, scanner2, rm, !sizes.at(sid1))
          }
        }
      }
      if (offset != null) {
        println(s"Detected offset between $sid1 and $sid2: ${offset.show}")
        // offset

        import Matrix._

        val direct = alloc[CStruct2[Coordinate, RotationMatrix]](1)
        direct._1 = offset
        direct._2 = rm
        val reverse = alloc[CStruct2[Coordinate, RotationMatrix]](1)
        reverse._1 = offset.copy.negate
        reverse._2 = rm.copy

        distancesAndRotations.set(sid1, sid2, direct)
        // distancesAndRotations.set(sid2, sid1, reverse)
      }

    } else null

  }

  def detectOffset(
      mapping: Ptr[Int],
      scanner1: Ptr[Coordinate],
      scanner2: Ptr[Coordinate],
      rm: RotationMatrix,
      size: Int
  )(implicit z: Zone) = {
    var offset: Coordinate = null
    var mismatch = false
    loops.breakable(0, size - 1, stopWhen = mismatch) { beaconId =>
      val matchingBeaconId = mapping(beaconId)
      if (matchingBeaconId != -1) {
        val right = rm.rotate(scanner2(mapping(beaconId)).copy)
        val left = scanner1(beaconId)

        if (offset == null) {
          offset = Coordinate.create(
            right.x - left.x,
            right.y - left.y,
            right.z - left.z
          )
        } else {
          mismatch ||= offset.x != (right.x - left.x)
          mismatch ||= offset.y != (right.y - left.y)
          mismatch ||= offset.z != (right.z - left.z)
          // println(
          //   s"$mismatch: ${offset.show} right = ${right.show}, left = ${left.show}"
          // )
        }
      }
    }

    if (mismatch) null else offset
  }

  def main(args: Array[String]): Unit = {

    val part_1_answer = {
      Zone.apply { implicit z =>
        val newScanners = WrappedArray.create[Coordinate]()
        val sizes = WrappedArray.create[Int]()
        val offsets = WrappedArray.create[Int]()

        var i = 0
        files.parsedLines(args.head) { case (l, parser) =>
          val isHeader = parser.const(c"--- scanner")().successful

          if (isHeader || l(0) == '\n') {
            if (l(0) == '\n') {
              if (offsets.size == 0) offsets.appendAndGrow(0)
              else
                offsets.appendAndGrow(
                  (!offsets.at(offsets.size - 1)) +
                    (!sizes.at(sizes.size - 1))
                )
              sizes.appendAndGrow(i)
            }
            i = 0
          } else {
            val coord = alloc[Coordinate](1)
            parser
              .rewind()
              .int(coord.at1)
              .const(c",")()
              .int(coord.at2)
              .const(c",")()
              .int(coord.at3)

            i += 1

            newScanners.appendAndGrow(coord)
          }
        }

        if (i != 0) {
          sizes.appendAndGrow(i)
          if (offsets.size == 0) offsets.appendAndGrow(0)
          else
            offsets.appendAndGrow(
              (!offsets.at(offsets.size - 1)) +
                (!sizes.at(sizes.size - 1))
            )
        }

        val scans = alloc[Coordinate](newScanners.size)

        newScanners.continuous(scans)
        offsets.foreach { of =>
          println(s"Offset: $of")
        }
        sizes.foreach { size =>
          println(s"size: $size")
        }

        val distancesAndRotations = Matrix
          .create[CStruct2[Coordinate, RotationMatrix]](sizes.size, sizes.size)
        import Matrix._

        loops.loop(0, sizes.size - 1) { sid1 =>
          loops.loop(0, sizes.size - 1) { sid2 =>
            if (sid1 != sid2) {
              println(s"Checking scanners $sid1 vs $sid2")
              detect(sid1, sid2, scans, offsets, sizes, distancesAndRotations)
            }

          }

        }

        // loops.loop(0, sizes.size - 1) { sid =>
        //   println(s"$sid: ${position(sid).show}")
        // }

        val positions = stackalloc[Coordinate](sizes.size)
        positions(0) = Coordinate.create(0, 0, 0)
        var noZeroes = false

        while (!noZeroes) {
          var foundZeroes = 0
          loops.loop(1, sizes.size - 1) { i =>
            if (positions(i).isZero) {
              foundZeroes += 1
              var found = -1
              var transition: Coordinate = null
              loops.breakable(0, sizes.size - 1, stopWhen = found != -1) { j =>
                transition = distancesAndRotations.unsafe(i, j)._1
                val reverseTransition = distancesAndRotations
                  .unsafe(j, i)
                  ._1

                if ((!positions(j).isZero || j == 0) && !transition.isZero) {
                  found = j
                } 
                // else if (
                //   (!positions(j).isZero || j == 0) && !reverseTransition.isZero
                // ) {
                //   found = j
                //   transition = reverseTransition.copy.negate
                // }
              }

              println(
                s"For $i we found $found and will apply ${transition.show}"
              )
              if (found != -1) {
                positions(i) = Coordinate.create(
                  positions(found).x + transition.x,
                  positions(found).y + transition.y,
                  positions(found).z + transition.z
                )
              }
            }
          }

          noZeroes = foundZeroes == 0
        }

        loops.loop(0, sizes.size - 1) { i =>
          println(s"Scanner $i is at ${positions(i).show}")
        }

        // while({
        //   loops.loop(1, sizes.size - 1) {i =>
        //     if(positions(i).isZero) {
        //     noZeroes = false;

        //     }
        //   }
        // }) {

        // }
        // def position(sid: Int): Coordinate = {
        //   if (sid == 0) Coordinate.create(0, 0, 0)
        //   else {
        //     val prev = position(sid - 1)
        //     val transition = distancesAndRotations.unsafe(sid - 1, sid)._1

        //     // println(s"$sid: ${prev.show} -- ${transition.show}")

        //     Coordinate.create(
        //       prev.x + transition.x,
        //       prev.y + transition.y,
        //       prev.z + transition.z
        //     )
        //   }
        // }

        loops.loop(0, sizes.size - 1) { sid1 =>
          loops.loop(0, sizes.size - 1) { sid2 =>
            if (sid1 != sid2) {
              val shift = distancesAndRotations.unsafe(sid1, sid2)._1
              val rotate = distancesAndRotations.unsafe(sid1, sid2)._2
              if (!shift.isZero)
                println(
                  s"$sid1 to $sid2: shift by ${shift.show}, rotate by ${rotate.show}, result is ${rotate.rotate(shift).show}"
                )
            }
          }
        }
      }
    }

    println(s"Part 1: $part_1_answer")

  }
}
