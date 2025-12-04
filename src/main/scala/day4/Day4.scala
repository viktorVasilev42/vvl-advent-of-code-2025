package day4

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

type Coordinate =  (Int, Int)

object Day4:
  private val inputFilename = "input.txt"

  private def readRollCoordinatesFromFile(fileName: String): Set[Coordinate] =
    Using(Source.fromResource(s"day4/$fileName")) {
      _.getLines().zipWithIndex.flatMap { case (line, y) =>
          line.zipWithIndex.collect { case ('@', x) => (x, y)}
        }
        .toSet
    }.get

  private def getNeighborsOfRoll(roll: Coordinate): List[Coordinate] =
    val (x, y) = roll
    (-1 to 1)
      .flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy)))
      .filter(_ != roll)
      .toList

  private def countNeighborRolls(rolls: Set[Coordinate], roll: Coordinate): Int =
    getNeighborsOfRoll(roll)
      .count(neighbor => rolls.contains(neighbor))

  private def accessibleRolls(rolls: Set[Coordinate]): Set[Coordinate] =
    rolls.filter(r => countNeighborRolls(rolls, r) < 4)

  @tailrec
  private def ultimatelyAccessibleRolls(rolls: Set[Coordinate], acc: Int): Int = accessibleRolls(rolls) match {
    case accessed if accessed.isEmpty => acc
    case accessed => ultimatelyAccessibleRolls(rolls removedAll accessed, acc + accessed.size)
  }

  def part1(): Int =
    val rolls = readRollCoordinatesFromFile(inputFilename)
    accessibleRolls(rolls).size

  def part2(): Int =
    val rolls = readRollCoordinatesFromFile(inputFilename)
    ultimatelyAccessibleRolls(rolls, 0)