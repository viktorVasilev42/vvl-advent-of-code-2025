package day7

import scala.collection.mutable
import scala.io.Source
import scala.util.Using
import MapState.Coordinate

object Day7 {
  private val inputFilename = "input.txt"

  private def readStartSplittersAndMapHeight(fileName: String): MapState =
    Using(Source.fromResource(s"day7/$fileName")) { src =>
      val lines = src.getLines().toList
      val mapHeight = lines.length
      val start = (lines.head.indexOf("S"), 0)
      val splitters = lines.zipWithIndex.flatMap { case (line, y) =>
          line.zipWithIndex.collect { case ('^', x) => (x, y) }
        }
        .toSet

      MapState(BeamsAndSplitters(Set(start), splitters), mapHeight)
    }.get

  private def countSplits(mapState: MapState): Long =
    if (mapState.beams.isEmpty) 0
    else
      val step = mapState.step()
      step.splits() + countSplits(mapState.newState(step.currentBeamsPlusGeneratedBeams()))

  private def timelines(mapState: MapState): Long =
    def inner(mapState: MapState, memoMap: mutable.Map[Set[Coordinate], Long]): Long =
      memoMap.getOrElseUpdate(mapState.beams, {
        val step = mapState.step()
        val leftState = mapState.newState(step.leftBeams())
        val rightState = mapState.newState(step.rightBeams())

        if ((leftState.beams ++ rightState.beams).nonEmpty)
          inner(leftState, memoMap) + inner(rightState, memoMap)
        else
          inner(mapState.newState(step.beams), memoMap)
      })

    val initMemo = mutable.Map[Set[Coordinate], Long]((Set(), 1L))
    inner(mapState, initMemo)

  def part1(): Long =
    countSplits(readStartSplittersAndMapHeight(inputFilename))

  def part2(): Long = {
    timelines(readStartSplittersAndMapHeight(inputFilename))
  }
}
