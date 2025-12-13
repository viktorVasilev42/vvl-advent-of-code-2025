package day8

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using


object Day8:
  private val inputFilename = "input.txt"

  private def readPointsFromFile(fileName: String): Array[Point] =
    Using(Source.fromResource(s"day8/$fileName")) {
      _.getLines
        .map(_.split(","))
        .map { case Array(x, y, z) => Point(x.toInt, y.toInt, z.toInt) }
        .toArray
    }.get

  @tailrec
  private def pollAndAddToCircuitsUntilDepth(queue: ConnectionQueue, circuits: List[Circuit], depth: Int): List[Circuit] =
    if (queue.isEmpty || depth == 0) circuits
    else
      val currConn = queue.dequeue()
      pollAndAddToCircuitsUntilDepth(queue, currConn.connectedToCircuits(circuits), depth - 1)

  @tailrec
  private def pollAndAddToCircuitsUntilUnified(queue: ConnectionQueue, circuits: List[Circuit], numPoints: Int): Long =
    val currConn = queue.dequeue()
    val newCircuits = currConn.connectedToCircuits(circuits)
    if (newCircuits.headOption.map(_.size).getOrElse(0) == numPoints)
        currConn.a.x.toLong * currConn.b.x.toLong
    else
      pollAndAddToCircuitsUntilUnified(queue, newCircuits, numPoints)

  private def multiplySizesOfThreeLargestCircuitsAfterNConnections(points: Array[Point], n: Int): Int =
    pollAndAddToCircuitsUntilDepth(Connection.createConnectionQueue(points), List(), n)
      .sortBy(-_.size)
      .take(3)
      .map(_.size)
      .product

  private def multiplyXCoordinatesOfLastConnectionThatUnifies(points: Array[Point]): Long =
    pollAndAddToCircuitsUntilUnified(Connection.createConnectionQueue(points), List(), points.length)


  def part1(): Int =
    val points = readPointsFromFile(inputFilename)
    multiplySizesOfThreeLargestCircuitsAfterNConnections(points, 1000)

  def part2(): Long =
    val points = readPointsFromFile(inputFilename)
    multiplyXCoordinatesOfLastConnectionThatUnifies(points)

