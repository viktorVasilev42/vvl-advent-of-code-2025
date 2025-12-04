package day4

import scala.io.Source
import scala.util.Using

object Day4:
  private val inputFilename = "input.txt"

  private def readRollCoordinatesFromFile(fileName: String): RollsState = {
    val rolls = Using(Source.fromResource(s"day4/$fileName")) {
      _.getLines().zipWithIndex.flatMap { case (line, y) =>
          line.zipWithIndex.collect { case ('@', x) => (x, y) }
        }
        .toSet
    }.get
    RollsState(rolls)
  }

  def part1(): Int =
    val rolls = readRollCoordinatesFromFile(inputFilename)
    rolls.accessibleRolls.size

  def part2(): Int =
    val rolls = readRollCoordinatesFromFile(inputFilename)
    rolls.ultimatelyAccessibleRolls