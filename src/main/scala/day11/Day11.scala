package day11

import scala.io.Source
import scala.util.Using

object Day11:
  private val inputFilename = "input.txt"
  private val youDevice: Device = "you"
  private val outDevice: Device = "out"
  private val svrDevice: Device = "svr"
  private val dacDevice: Device = "dac"
  private val fftDevice: Device = "fft"

  private def readDevicesAndConnections(fileName: String): DeviceGraph =
    Using(Source.fromResource(s"day11/$fileName")) { src =>
      val connections = src.getLines
        .map(_.split(":*\\s").toList)
        .collect { case from :: tos => (from, tos) }
        .toMap
      DeviceGraph(connections)
    }.get

  def part1(): Long =
    val deviceOrders = List(
      List(youDevice, outDevice)
    )
    readDevicesAndConnections(inputFilename).countAllPathsInDeviceOrders(deviceOrders)

  def part2(): Long =
    val deviceOrders = List(
      List(svrDevice, dacDevice, fftDevice, outDevice),
      List(svrDevice, fftDevice, dacDevice, outDevice)
    )
    readDevicesAndConnections(inputFilename).countAllPathsInDeviceOrders(deviceOrders)
