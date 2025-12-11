package day6

import ProblemParser.readAlignedNumStringsAndOperationsFromFile
import StringPadder.tailPadColumn

object Day6 {
  private val inputFilename = "input.txt"

  private def getVerticalNumbers(column: List[String]): List[Long] = column.head match
      case "" => List()
      case _ =>
        column.map(_.head).mkString.strip.toLong :: getVerticalNumbers(column.map(_.tail))

  private def getNextColumn(nums: List[List[String]]): List[String] =
    nums.map(_.head)

  private def getRestColumns(nums: List[List[String]]): List[List[String]] =
    nums.map(_.tail)

  private def sumOfProblems(nums: List[List[String]], ops: List[Char]): Long = ops match
    case Nil => 0L
    case '+' :: restOps =>
      getNextColumn(nums).map(_.strip.toLong).sum + sumOfProblems(getRestColumns(nums), restOps)
    case '*' :: restOps =>
      getNextColumn(nums).map(_.strip.toLong).product + sumOfProblems(getRestColumns(nums), restOps)

  private def sumOfVerticalProblems(nums: List[List[String]], ops: List[Char]): Long = ops match
    // numbers in last column (problem) must be padded with whitespace on the right side
    case '+' :: Nil =>
      getVerticalNumbers(tailPadColumn(getNextColumn(nums))).sum
    case '*' :: Nil =>
      getVerticalNumbers(tailPadColumn(getNextColumn(nums))).product
    // else everything is already padded
    case '+' :: restOps =>
      getVerticalNumbers(getNextColumn(nums)).sum + sumOfVerticalProblems(getRestColumns(nums), restOps)
    case '*' :: restOps =>
      getVerticalNumbers(getNextColumn(nums)).product + sumOfVerticalProblems(getRestColumns(nums), restOps)


  def part1(): Long =
    val (nums, ops) = readAlignedNumStringsAndOperationsFromFile(inputFilename)
    sumOfProblems(nums, ops)

  def part2(): Long =
    val (nums, ops) = readAlignedNumStringsAndOperationsFromFile(inputFilename)
    sumOfVerticalProblems(nums, ops)
}
