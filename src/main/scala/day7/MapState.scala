package day7

import scala.util.chaining.scalaUtilChainingOps

object MapState:
  type Coordinate = (Int, Int)

case class MapState(beamsAndSplitters: BeamsAndSplitters, height: Long):
  import MapState.Coordinate

  def beams: Set[Coordinate] =
    this.beamsAndSplitters.beams

  def splitters: Set[Coordinate] =
    this.beamsAndSplitters.splitters

  def newState(beams: Set[Coordinate]) =
    MapState(BeamsAndSplitters(beams, this.splitters), this.height)

  def step(): BeamsAndSplitters =
    beams.map((x, y) => (x, y + 1)).partition(splitters.contains)
      .pipe((splitters, beams) => BeamsAndSplitters(beams.filter((_, y) => y < height), splitters))
