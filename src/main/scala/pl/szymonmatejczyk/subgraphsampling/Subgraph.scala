package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph.DirectedGraph

import scala.collection.immutable.BitSet

case class Subgraph(graph: DirectedGraph, nodes: collection.Set[Int]) {
  def neighbors(): collection.Set[Int] = {
    val nodesSet = BitSet() ++ nodes
    nodes.flatMap(graph.getNodeById).flatMap(_.outboundNodes()).filter(x => !nodesSet.contains(x))
  }

  def size = nodes.size
}
