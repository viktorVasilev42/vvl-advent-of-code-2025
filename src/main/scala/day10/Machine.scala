package day10

import scala.io.Source
import scala.util.Using

case class Machine(lights: List[Boolean], buttons: List[Button], joltages: List[Int]):
  def indicesOfOnAndOffLights(): (List[Int], List[Int]) =
    lights.zipWithIndex.partitionMap{
      case (true, idx) => Left(idx)
      case (false, idx) => Right(idx)
    }

object Machine:
  private def apply(lightsStr: String, buttonsStrArr: Array[String], joltagesStr: String): Machine =
    val lights = lightsStr.map {
      case '#' => true
      case _ => false
    }.toList
    val buttons = buttonsStrArr.map(_.split(",").map(_.toInt).toSet).toList
    val joltages = joltagesStr.split(",").map(_.toInt).toList
    Machine(lights, buttons, joltages)

  def readMachinesFromFile(fileName: String): List[Machine] =
    Using(Source.fromResource(s"day10/$fileName")) {
      _.getLines
        .map(line => line.splitAt(line.indexOf("(")))
        .map((lights, buttonsAndJoltages) => {
          val Array(buttons, joltages) = buttonsAndJoltages.split("\\{")
          (
            lights.drop(1).takeWhile(_ != ']'),
            buttons.strip.split("\\s+").map(_.drop(1).takeWhile(_ != ')')),
            joltages.strip.takeWhile(_ != '}')
          )
        })
        .map((lights, buttons, joltages) => Machine(lights, buttons, joltages))
        .toList
    }.get
