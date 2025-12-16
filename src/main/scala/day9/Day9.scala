package day9

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day9:
  private val inputFilename = "input.txt"

  private def readRedTilesFromFile(fileName: String): List[Point] =
    Using(Source.fromResource(s"day9/$fileName")) {
      _.getLines
        .map(_.split(","))
        .map{ case Array(x, y) => Point(x.toLong, y.toLong) }
        .toList
    }.get

  private def getMinMaxXOfPoints(points: Seq[Point]): (Long, Long) =
    (points.map(_.x).min, points.map(_.x).max)

  private def redAndGreenPolygon(points: List[Point]): Set[Point] =
    Set.from(points ::: points.flatMap(_.getGreenTilesToNeighbors(points.toSet)))

  private def largestArea(points: List[Point]): Long = points match
    case Nil | _ :: Nil => 0
    case curr :: rest => Math.max(rest.map(_.areaToPoint(curr)).max, largestArea(rest))

  private def getPointsOfRectangle(p1: Point, p2: Point): List[Point] =
    val (minX, maxX) = if (p1.x <= p2.x) (p1.x, p2.x) else (p2.x, p1.x)
    val (minY, maxY) = if (p1.y <= p2.y) (p1.y, p2.y) else (p2.y, p1.y)

    val chosenY = if minY != maxY then minY + 1 else minY
     List(
       Point(p1.x, p2.y), Point(p2.x, p1.y), Point(minX + (maxX - minX) / 2, chosenY),
       Point(minX + (maxX - minX) / 2, minY), Point(minX + (maxX - minX) / 2, maxY)
     )

  private def largestAreaInPolygon(points: Seq[Point], polygon: Set[Point]): Long =
    @tailrec
    def largestAreaInPolygonLoop(points: Seq[Point], polygon: Set[Point], minX: Long, maxX: Long,
                                     memoizedInside: mutable.Map[Point, Boolean], accMax: Long): Long =
      if points.size < 2 then accMax
      else
        val (curr, rest) = (points.head, points.tail)

        val maxVal = rest.foldLeft(0L) { (best, p) =>
          if getPointsOfRectangle(p, curr).forall(_.isInsidePolygon(polygon, minX, maxX, memoizedInside)) then
            val area = p.areaToPoint(curr)
            if area > best then area else best
          else best
        }
        largestAreaInPolygonLoop(rest, polygon, minX, maxX, memoizedInside, Math.max(maxVal, accMax))

    val (minX, maxX) = getMinMaxXOfPoints(points)
    largestAreaInPolygonLoop(points, polygon, minX, maxX, mutable.Map(), 0L)

  def part1(): Long =
   largestArea(readRedTilesFromFile(inputFilename))

  def part2(): Long =
    val points = readRedTilesFromFile(inputFilename)
      .filter(p => p.y > 30_000 && p.y < 70_000)
    largestAreaInPolygon(points, redAndGreenPolygon(points))
