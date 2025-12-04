package day3

import scala.io.Source
import scala.util.Using

object Day3 {
  private val inputFilename = "input.txt"

  private def readBanksFromFile(fileName: String): List[String] =
    Using(Source.fromResource(s"day3/$fileName")){ _.getLines.toList }.get

  private def restOfBankFromDigit(str: String, digitToFind: Long): String =
    str.dropWhile(_ != (digitToFind + '0').toChar)

  private def largestJoltage(bank: String, depth: Int): String = {
    (9 to 1 by -1)
      .find(digit => restOfBankFromDigit(bank, digit) match {
        case "" => false
        case _ if depth == 1 => true
        case rest => largestJoltage(rest.drop(1), depth - 1) != "0"
      })
      .map(digit => depth match {
        case 1 => digit.toString
        case x => digit.toString ++ largestJoltage(restOfBankFromDigit(bank, digit).drop(1), depth - 1)
      })
      .getOrElse("0")
  }

  private def turnOnBatteries(numBatteries: Int): Long =
    readBanksFromFile(inputFilename)
      .map(bank => largestJoltage(bank, numBatteries).toLong)
      .sum

  def part1(): Long =
    turnOnBatteries(2)

  def part2(): Long =
    turnOnBatteries(12)
}
