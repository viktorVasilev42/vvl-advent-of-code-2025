package day8

import scala.collection.mutable

type Circuit = mutable.Set[Point]

case class Point(x: Int, y: Int, z: Int):
  private def squaredDist(a: Point, b: Point): Long =
    (Math.pow(a.x - b.x, 2) + Math.pow(a.y - b.y, 2) + Math.pow(a.z - b.z, 2)).toLong

  def connect(o: Point): Connection =
    Connection(this, o, squaredDist(this, o))
