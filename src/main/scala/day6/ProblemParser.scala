package day6

import scala.io.Source
import scala.util.Using
import StringPadder.readPaddedNumbersFromLine

object ProblemParser {

  private def readNumsAndOpsFromFile(fileName: String): (List[String], String) =
    Using(Source.fromResource(s"day6/$fileName")) { src =>
      val (nums, ops) = src.getLines().span(line => !(line.startsWith("+") || line.startsWith("*")))
      (nums.toList, ops.next())
    }.get

  def readAlignedNumStringsAndOperationsFromFile(fileName: String): (List[List[String]], List[Char]) =
    val (numLines, opsLine) = readNumsAndOpsFromFile(fileName)
    val indexedOps = opsLine.zipWithIndex.filter((ch, _) => "+*".contains(ch)).toList
    val paddedNumbers = numLines.map(readPaddedNumbersFromLine(_, indexedOps.map(_._2)))
    (paddedNumbers, indexedOps.map(_._1))
}
