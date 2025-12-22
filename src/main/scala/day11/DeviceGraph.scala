package day11

import scala.collection.mutable

type Device = String
type Path = Set[Device]

case class DeviceGraph(connections: Map[Device, List[Device]]):
  private def adj(d: Device) =
    connections.getOrElse(d, Nil)

  private val topoSortList: List[Device] =
    var visited = Set.empty[Device]
    var result = List.empty[Device]

    def dfs(d: Device): Unit =
      if (!visited(d)) {
        visited += d
        val neighbors = adj(d)
        neighbors.foreach(dfs)
        result = d :: result
      }

    val vertices: Set[Device] = connections.keySet ++ connections.values.flatten
    vertices.foreach(dfs)
    result

  private def countPathsFromTo(from: Device, to: Device): Long =
    val pathCounts = mutable.LinkedHashMap.from(topoSortList.map(dev => (dev, 0L)))
    pathCounts(from) = 1L
    pathCounts.foreach((dev, count) => adj(dev).foreach(adjDev => pathCounts(adjDev) += count))
    pathCounts(to)

  def countAllPathsInDeviceOrders(deviceOrders: List[List[Device]]): Long =
    def countPaths(deviceOrder: List[Device]): Long =
      deviceOrder.foldLeft((1L, deviceOrder.head)) { case ((acc, prev), next) =>
        (acc * this.countPathsFromTo(prev, next), next)
      }._1

    deviceOrders.foldLeft(0L){ (acc, order) => acc + countPaths(order) }
