package day2

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day2 {
  private val inputFilename = "input.txt"

  private def readRangesFromFile(fileName: String): List[(Long, Long)] =
    Using(Source.fromResource(s"day2/$fileName")) { source =>
      val firstLine = source.getLines().next()
      firstLine.split(",")
        .map(_.split("-"))
        .map { case Array(start, end) => (start.toLong, end.toLong) }
        .toList
    }.get

  private def hasEqualHalves(num: Long): Boolean = {
    val numString = num.toString
    val mid = numString.length / 2
    numString.length % 2 == 0 &&
      numString.substring(0, mid) == numString.substring(mid)
  }

  private def allSubstringsOfString(str: String): List[String] =
    (1 to str.length)
      .map(idx => str.substring(0, idx))
      .toList

  @tailrec
  private def isTargetStringRepetitionOfSubstrings(target: String, substr: String): Boolean = target match {
    case "" => true
    case str if str.length < substr.length => false
    case str =>
      val (partWithLengthAsSubstr, rest) = str.splitAt(substr.length)
      partWithLengthAsSubstr == substr && isTargetStringRepetitionOfSubstrings(rest, substr)
  }

  private def isNumberRepetitionOfNumbers(num: Long): Boolean =
    val numString = num.toString
    val firstHalf = numString.take(numString.length / 2)
    allSubstringsOfString(firstHalf)
      .exists(isTargetStringRepetitionOfSubstrings(numString, _))


  def part1(): Long =
    readRangesFromFile(inputFilename)
      .flatMap((start, end) => (start to end).filter(hasEqualHalves))
      .sum

  def part2(): Long = {
    readRangesFromFile(inputFilename)
      .flatMap((start, end) => (start to end).filter(isNumberRepetitionOfNumbers))
      .sum
  }
}
