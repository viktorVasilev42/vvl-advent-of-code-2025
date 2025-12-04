package day4

import scala.annotation.tailrec

type Coordinate =  (Int, Int)

case class RollsState(allRolls: Set[Coordinate]):
  private def countNeighborRolls(roll: Coordinate): Int =
    RollsState.getNeighborsOfRoll(roll)
      .count(neighbor => allRolls.contains(neighbor))

  def accessibleRolls: Set[Coordinate] =
    allRolls.filter(r => countNeighborRolls(r) < 4)

  def ultimatelyAccessibleRolls: Int =
    RollsState.accUltimatelyAccessibleRolls(allRolls, 0)

object RollsState:
  private def getNeighborsOfRoll(roll: Coordinate): List[Coordinate] =
    val (x, y) = roll
    (-1 to 1)
      .flatMap(dx => (-1 to 1).map(dy => (x + dx, y + dy)))
      .filter(_ != roll)
      .toList

  @tailrec
  private def accUltimatelyAccessibleRolls(rolls: Set[Coordinate], acc: Int): Int = RollsState(rolls).accessibleRolls match {
    case accessed if accessed.isEmpty => acc
    case accessed => accUltimatelyAccessibleRolls(rolls removedAll accessed, acc + accessed.size)
  }
