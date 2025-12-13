package day7

import MapState.Coordinate

case class BeamsAndSplitters(beams: Set[Coordinate], splitters: Set[Coordinate]):
  def leftBeams(): Set[Coordinate] =
    splitters.map((x, y) => (x - 1, y))

  def rightBeams(): Set[Coordinate] =
    splitters.map((x, y) => (x + 1, y))

  def generatedBeams(): Set[Coordinate] =
    (leftBeams() zip rightBeams()).flatMap((left, right) => List(left, right))

  def currentBeamsPlusGeneratedBeams(): Set[Coordinate] =
    beams ++ generatedBeams()

  def splits(): Long =
    splitters.size
