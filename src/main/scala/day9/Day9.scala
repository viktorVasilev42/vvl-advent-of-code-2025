package day9

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

case class Point(x: Long, y: Long):
  def areaToPoint(o: Point): Long =
    (Math.abs(this.x - o.x) + 1) * (Math.abs(this.y - o.y) + 1)

  private def leftNeighbor(points: Set[Point]): Option[Point] =
    (this.x - 1 to 0 by -1).map(Point(_, this.y)).find(points.contains)

  private def downNeighbor(points: Set[Point]): Option[Point] =
    (this.y - 1 to 0 by -1).map(Point(this.x, _)).find(points.contains)

  private def toNeighbor(o: Option[Point]): List[Point] = o match
    case None => List()
    case Some(o) =>
      if (this.x == o.x)
        (o.y + 1 until this.y).map(Point(x, _)).toList
      else
        (o.x + 1 until this.x).map(Point(_, y)).toList

  def getGreenTilesToNeighbors(points: Set[Point]): List[Point] =
    toNeighbor(leftNeighbor(points)) ::: toNeighbor(downNeighbor(points))

  def isInsidePolygon(polygon: Set[Point], minX: Long, maxX: Long, memoizedInside: mutable.Map[Point, Boolean]): Boolean =
    def countRayCuts(ray: Seq[Point]): Int = {
      ray.zipWithIndex.count((pnt, idx) => polygon.contains(pnt) && (
        (idx == 0 || !polygon.contains(ray(idx - 1))) || (idx == ray.length - 1 || !polygon.contains(ray(idx + 1)))))
    }

    if (polygon.contains(this)) true
    else if (memoizedInside.contains(this)) memoizedInside(this)
    else
      val range = if (this.x - minX) <= (maxX - this.x) then (minX to this.x) else (this.x to maxX)
      val ray = range.map(Point(_, this.y)).toArray
      val isInside = countRayCuts(ray) % 2 != 0
      memoizedInside += (this -> isInside)
      isInside

object Day9:
  private val inputFilename = "input.txt"

  private def readRedTilesFromFile(fileName: String): List[Point] =
    Using(Source.fromResource(s"day9/$fileName")) {
      _.getLines
        .map(_.split(","))
        .map{ case Array(x, y) => Point(x.toLong, y.toLong) }
        .toList
    }.get

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

  @tailrec
  private def largestAreaInPolygon(points: Seq[Point], polygon: Set[Point], minX: Long, maxX: Long,
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
      largestAreaInPolygon(rest, polygon, minX, maxX, memoizedInside, Math.max(maxVal, accMax))

  def part1(): Long =
   largestArea(readRedTilesFromFile(inputFilename))

  def part2(): Long =
    val points = readRedTilesFromFile(inputFilename)
      .filter(p => p.y > 30_000 && p.y < 70_000)

    val redAndGreenPolygon = mutable.Set[Point]()
    redAndGreenPolygon ++= points
    redAndGreenPolygon ++= points.flatMap(_.getGreenTilesToNeighbors(redAndGreenPolygon.toSet))

    val minX = points.map(_.x).min
    val maxX = points.map(_.x).max
    largestAreaInPolygon(points, redAndGreenPolygon.toSet, minX, maxX, mutable.Map(), 0L)
