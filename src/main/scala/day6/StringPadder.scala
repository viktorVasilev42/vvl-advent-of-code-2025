package day6

import scala.annotation.tailrec

object StringPadder {

  @tailrec
  private def tailPadStringToLength(str: String, targetLength: Int): String =
    if (str.length == targetLength) str
    else tailPadStringToLength(str ++ " ", targetLength)

  def tailPadColumn(column: List[String]): List[String] =
    column.map(tailPadStringToLength(_, column.maxBy(_.length).length))

  def readPaddedNumbersFromLine(line: String, opIndexes: List[Int]): List[String] = {
    def readNum(line: String, opIndexes: List[Int], currIndex: Int): List[String] =
      opIndexes match
        case Nil => line.substring(currIndex) :: Nil
        case idx :: rest => line.substring(currIndex, idx - 1) :: readNum(line, rest, idx)

    readNum(line, opIndexes.drop(1), 0)
  }
}
