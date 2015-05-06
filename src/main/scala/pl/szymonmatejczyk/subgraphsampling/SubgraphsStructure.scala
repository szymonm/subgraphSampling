package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph.GraphDir.GraphDir
import com.twitter.cassovary.graph.{DepthFirstTraverser, DirectedGraph, GraphDir}

import scala.collection.mutable

class SubgraphsStructure(graph: DirectedGraph) {
  type NodeId = Int
  def isConnected(subgraph: collection.Set[NodeId], dir: GraphDir = GraphDir.OutDir): Boolean = {
    if (subgraph.isEmpty) {
      true
    } else {
      val dfs = new DepthFirstTraverser(graph, dir, Seq(subgraph.head)) with NodesFilter {
        override def nodeFilter: (NodeId) => Boolean = nodeId => subgraph.contains(nodeId)
      }
      subgraph.size == dfs.size
    }
  }

  def components(nodes: collection.Set[NodeId], dir: GraphDir = GraphDir.OutDir): collection.Set[Set[NodeId]] = {
    val components = mutable.Set[Set[NodeId]]()
    val nodesLeft = mutable.Set[NodeId]() ++ nodes
    while (nodesLeft.nonEmpty) {
      val dfs = new DepthFirstTraverser(graph, dir, Seq(nodesLeft.head)) with NodesFilter {
        override def nodeFilter: (NodeId) => Boolean = nodeId => nodes.contains(nodeId)
      }
      val set = dfs.map(_.id).toSet
      components += set
      nodesLeft --= set
    }
//    Predef.assert(components.flatten == nodes)
    components
  }
}
