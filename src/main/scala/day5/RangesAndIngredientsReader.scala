package day5

import scala.io.Source
import scala.util.Using

object RangesAndIngredientsReader {
  private def getRanges(lines: Iterator[String]): List[Range] =
    lines
      .map(_.split("-"))
      .map { case Array(start, end) => (start.toLong, end.toLong) }
      .toList

  private def getIngredients(lines: Iterator[String]): List[Long] =
    lines.drop(1).map(_.toLong).toList

  def readFromFile(fileName: String): (List[Range], List[Long]) =
    Using(Source.fromResource(s"day5/$fileName")) { src =>
      val (ranges, ingr) = src.getLines.span(_.nonEmpty)
      (getRanges(ranges), getIngredients(ingr))
    }.get

}
