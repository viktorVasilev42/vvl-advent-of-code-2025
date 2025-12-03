package day1

import scala.io.Source
import scala.util.Using

object Day1:
  private val initDial = 50
  private val numSafeTicks = 100
  private val inputFilename = "input.txt"
  private val allInstructions = readInstructionsFromFile(inputFilename)

  private def readInstructionsFromFile(fileName: String): List[SafeInstruction] =
    Using(Source.fromResource(s"day1/$fileName")) { source =>
      val allLines = source.getLines()
      allLines.map(line => {
        val currHead = line.headOption getOrElse ' '
        val currTail = if (line.length > 1) line.tail else ""
        SafeInstruction(currHead, currTail)
      }).toList
    }.get

  private def positiveMod(num: Int, mod: Int): Int =
    (num % mod + mod) % mod

  private def getDialValueAsOneToHundred(dial: Int) =
    if (dial == 0) 100 else dial

  
  def part1(): Int =
    val (_, finalResult) = allInstructions
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
    val (_, finalResult) = allInstructions
      .map(_.getIntValue)
      .collect { case Some(num) => num }
      .foldLeft((initDial, 0)) {
        case ((currDial, currResult), num) =>
          val rawNewDial = currDial + num
          val newDial = positiveMod(rawNewDial, numSafeTicks)

          val resultIncrement = if (num > 0)
            rawNewDial / numSafeTicks
          else
            (numSafeTicks - getDialValueAsOneToHundred(currDial) - num) / numSafeTicks

          (newDial, currResult + resultIncrement)
      }
    finalResult
