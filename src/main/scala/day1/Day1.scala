package day1

import scala.io.Source

case class SafeInstruction(direction: Char, strValue: String):
  def getIntValue: Option[Int] = this match
    case SafeInstruction('L', stringVal) => Some(-stringVal.toInt)
    case SafeInstruction('R', stringVal) => Some(stringVal.toInt)
    case _ => None
    

object Day1:
  private val initDial = 50
  private val numSafeTicks = 100

  def readInstructionsFromFile(fileName: String): List[SafeInstruction] =
    val allLines = Source.fromResource(s"day1/$fileName").getLines()
    allLines.map((line) => {
      val currHead = line.headOption getOrElse ' '
      val currTail = if (line.length > 1) line.tail else ""
      SafeInstruction(currHead, currTail)
    }).toList

  def positiveMod(num: Int, mod: Int): Int =
    ((num % mod + mod) % mod)

  def getDialValueAsOneToHundred(dial: Int) =
    if (dial == 0) 100 else dial

  
  def part1(): Int =
    val (_, finalResult) = readInstructionsFromFile("input.txt")
      .map(_.getIntValue)
      .collect { case Some(num) => num }
      .foldLeft((initDial, 0)) {
        case ((currDial, currResult), num) =>
          val newDial = positiveMod(currDial + num, numSafeTicks)
          val newResult = currResult + (if (newDial == 0) 1 else 0)
          (newDial, newResult)
      }
    finalResult

  def part2(): Int =
    val (_, finalResult) = readInstructionsFromFile("input.txt")
      .map(_.getIntValue)
      .collect { case Some(num) => num }
      .foldLeft((initDial, 0)) {
        case ((currDial, currResult), num) =>
          val rawNewDial = currDial + num
          val newDial = positiveMod(rawNewDial, numSafeTicks)

          val resultIncrement = if (num > 0)
            (rawNewDial / numSafeTicks)
          else
            ((numSafeTicks - getDialValueAsOneToHundred(currDial) - num) / numSafeTicks)

          (newDial, currResult + resultIncrement)
      }
    finalResult
