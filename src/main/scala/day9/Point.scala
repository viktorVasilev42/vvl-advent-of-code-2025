package day9

import scala.collection.mutable

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
