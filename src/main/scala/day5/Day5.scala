package day5

import scala.collection.mutable.ArrayBuffer
import RangesAndIngredientsReader.readFromFile

type Range = (Long, Long)

object Day5 {
  private val inputFilename = "input.txt"

  private def isFresh(ingr: Long, ranges: List[Range]): Boolean =
    ranges.exists((start, end) => ingr >= start && ingr <= end)

  private def subsumeNewRangeInOldRange(oldRange: Range, newRange: Range): Option[Range] =
    val (oldStart, oldEnd) = oldRange

    newRange match {
      case (start, end) if start > oldEnd || end < oldStart => None // outside
      case (start, end) if start >= oldStart && end <= oldEnd => Some(oldRange) // subsume
      case (start, end) if start >= oldStart && end > oldEnd => Some(oldStart, end) // start in, end out
      case (start, end) if start < oldStart && end <= oldEnd => Some(start, oldEnd) // start out, end in
      case (start, end) if start < oldStart && end > oldEnd => Some(newRange) // parent
      case (_, _) => None
    }

  private def sortRanges(ranges: List[Range]) =
    ranges.sortBy((start, _) => start)

  private def simplifyOverlappingRanges(ranges: List[Range]) =
    sortRanges(ranges).foldLeft(ArrayBuffer.empty[Range]) { (acc, newRange) =>
        var allEmpty = true

        acc.mapInPlace(oldRange =>
          subsumeNewRangeInOldRange(oldRange, newRange) match {
            case None => oldRange
            case Some(updatedRange) =>
              allEmpty = false
              updatedRange
          }
        )

        if (allEmpty && !acc.contains(newRange)) acc += newRange else acc
      }
      .toList

  private def countNumbersInListOfRanges(ranges: List[Range]): Long =
    ranges.foldLeft(0L) { case (acc, (start, end)) =>
      acc + (end - start + 1)
    }

  def part1(): Long =
    val (fileRanges, ingr) = readFromFile(inputFilename)
    ingr.count(isFresh(_, fileRanges))

  def part2(): Long =
    val (fileRanges, _) = readFromFile(inputFilename)
    countNumbersInListOfRanges(simplifyOverlappingRanges(fileRanges))
}
