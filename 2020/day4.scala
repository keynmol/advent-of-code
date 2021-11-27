// using scala 2.13.6
// using scala-native

import scala.scalanative.unsafe._
import scala.scalanative.unsigned._
import scalanative.libc.stdio
import scala.scalanative.libc.stdlib
import scala.scalanative.runtime.libc
import scala.scalanative.libc.string
import scala.scalanative.annotation.alwaysinline

object Day4 {
  /*
    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

    PART 2:
    byr (Birth Year) - four digits; at least 1920 and at most 2002.
    iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    hgt (Height) - a number followed by either cm or in:
        If cm, the number must be at least 150 and at most 193.
        If in, the number must be at least 59 and at most 76.
    hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    pid (Passport ID) - a nine-digit number, including leading zeroes.
    cid (Country ID) - ignored, missing or not.

   */
  val MAX_LINE_LENGTH = 100

  object Flags {
    val byr = 1
    val iyr = 2
    val eyr = 4
    val hgt = 8
    val hcl = 16
    val ecl = 32
    val pid = 64
    val cid = 128

    def of(cstr: CString): Int = {
      @alwaysinline def is(what: CString) = string.strcmp(cstr, what) == 0

      if (is(c"byr")) Flags.byr
      else if (is(c"iyr")) Flags.iyr
      else if (is(c"eyr")) Flags.eyr
      else if (is(c"hgt")) Flags.hgt
      else if (is(c"hcl")) Flags.hcl
      else if (is(c"ecl")) Flags.ecl
      else if (is(c"pid")) Flags.pid
      else if (is(c"cid")) Flags.cid
      else 0
    }
  }

  def solve(filename: CString, strict: Boolean)(implicit z: Zone) = {
    val file = stdio.fopen(filename, c"r")
    val line = stackalloc[CChar](MAX_LINE_LENGTH)
    val field = stackalloc[CChar](MAX_LINE_LENGTH)
    val endChar = stackalloc[CChar]
    val value = stackalloc[CChar](MAX_LINE_LENGTH)
    val consumed = stackalloc[CInt]
    var setFlags = 0 // all bits set

    var validPassports = 0

    val numValue = stackalloc[CInt] // used in part two
    while (stdio.fgets(line, MAX_LINE_LENGTH, file) != null) {
      val len = string.strlen(line).toInt
      var remaining = len
      var parsedWithSpace = true

      if (len == 1) {

        if (setFlags == 255 || setFlags == (255 ^ Flags.cid)) {
          // println(s"Valid passport: $setFlags")
          validPassports += 1
        }
        setFlags = 0
        // println("----")
      } else {

        while (remaining > 0 /* line ending*/ && parsedWithSpace) {
          parsedWithSpace = stdio.sscanf(
            line + (len - remaining),
            c"%3[^:]:%s%n",
            field,
            value,
            consumed
          ) == 2

          remaining -= (!consumed + 1).toInt

          if (string.strcmp(field, c"byr") == 0) {
            val validated =
              (stdio.sscanf(
                value,
                c"%4d",
                numValue
              ) == 1) && (!numValue) >= 1920 && (!numValue) <= 2002

            if (!strict || validated)
              setFlags |= Flags.byr
          } else if (string.strcmp(field, c"pid") == 0) {
            lazy val validated =
              string.strlen(value).toInt == 9 &&
              (stdio.sscanf(
                value,
                c"%9d",
                numValue
              ) == 1)

            if (!strict || validated)
              setFlags |= Flags.pid
          } else if (string.strcmp(field, c"eyr") == 0) {
            lazy val validated =
              string.strlen(value).toInt == 4 &&
              (stdio.sscanf(
                value,
                c"%4d",
                numValue
              ) == 1) && (!numValue) >= 2020 && (!numValue) <= 2030

            if (!strict || validated)
              setFlags |= Flags.eyr

          } else if (string.strcmp(field, c"hcl") == 0) {
            // stdio.printf(c"Color: %s\n", value)

            lazy val validated = {
              string.strlen(value).toInt == 7 &&
              (stdio.sscanf(
                value,
                c"#%[0-9a-f]%n",
                value,
                numValue
              ) == 1) && (!numValue == 7)

            }
            if (!strict || validated)
              setFlags |= Flags.hcl
          } else if (string.strcmp(field, c"iyr") == 0) {
            lazy val validated =
              string.strlen(value).toInt == 4 &&
              (stdio.sscanf(
                value,
                c"%4d",
                numValue
              ) == 1) && (!numValue) >= 2010 && (!numValue) <= 2020

            if (!strict || validated)
              setFlags |= Flags.iyr
          } else if (string.strcmp(field, c"cid") == 0) setFlags |= Flags.cid
          else if (string.strcmp(field, c"hgt") == 0) {
            lazy val validated = {
              val result = stdio.sscanf(value, c"%d", numValue)
              (!numValue >= 150 && !numValue <= 193 && string.strcmp(
                value + 3,
                c"cm"
              ) == 0) ||
              (!numValue >= 59 && !numValue <= 76 && string.strcmp(
                value + 2,
                c"in"
              ) == 0)
            }
            if (!strict || validated)
              setFlags |= Flags.hgt
          } else if (string.strcmp(field, c"ecl") == 0) {
            lazy val validated = {
              import string.{strcmp => s}

              s(value, c"amb") == 0 || s(value, c"blu") == 0 ||
              s(value, c"grn") == 0 || s(value, c"hzl") == 0 ||
              s(value, c"gry") == 0 || s(value, c"brn") == 0 ||
              s(value, c"oth") == 0
            }
            if (!strict || validated)
              setFlags |= Flags.ecl
          } else {
            stdio.printf(c"Unknown field: %s\n", field)
          }
          // THIS is useful for debugging
          //
          // if (
          //   string.strcmp(field, c"pid") == 0 && (setFlags & Flags.of(
          //     field
          //   )) == Flags.of(field)
          // )
          //   stdio.printf(
          //     c"Valid: %d, Field: %s, value: %s\n",
          //     (setFlags & Flags.of(field)) == Flags.of(field),
          //     field,
          //     value
          //   )

        }
      }

    }

    if (setFlags == 255 || setFlags == (255 ^ Flags.cid))
      validPassports += 1

    stdio.fclose(file)

    validPassports
  }

  def main(args: Array[String]): Unit = Zone { implicit z =>
    val filename = toCString(args.head)

    println(s"Part 1: ${solve(filename, strict = false)}")
    println(s"Part 2: ${solve(filename, strict = true)}")
  }
}
