package day8

import scala.collection.mutable

type ConnectionQueue = mutable.PriorityQueue[Connection]

case class Connection(a: Point, b: Point, dist: Long):
  def connectedToCircuits(circuits: List[Circuit]): List[Circuit] =
    (circuits.find(_ contains this.a), circuits.find(_ contains this.b)) match
      case (None, None) => mutable.Set(this.a, this.b) :: circuits
      case (Some(crctA), Some(crctB)) if crctA == crctB => circuits
      case (Some(crctA), Some(crctB)) =>
        crctA += this.b ++= crctB
        circuits.filterNot(_ == crctB)
      case (Some(crctA), None) =>
        crctA += this.b
        circuits
      case (None, Some(crctB)) =>
        crctB += this.a
        circuits

object Connection:
  def createConnectionQueue(points: Array[Point]): ConnectionQueue =
    val connections = points.indices
      .flatMap(i => (i + 1 until points.length).map((i, _)))
      .map((i, j) => points(i).connect(points(j)))
    mutable.PriorityQueue(connections*)

implicit val connectionOrder: Ordering[Connection] =
  Ordering.by[Connection, Long](_.dist).reverse
