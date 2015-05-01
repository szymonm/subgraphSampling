import com.twitter.cassovary.graph._
import pl.szymonmatejczyk.subgraphsampling.ParentTracker

import scala.collection.mutable

val nodeSeqIterator = Seq(
  NodeIdEdgesMaxId(10, Array(11, 12)),
  NodeIdEdgesMaxId(11, Array(10, 13)),
  NodeIdEdgesMaxId(12, Array(10, 11)),
  NodeIdEdgesMaxId(13, Array(11, 14, 16)),
  NodeIdEdgesMaxId(14, Array(13, 14)),
  NodeIdEdgesMaxId(15, Array(14, 16)),
  NodeIdEdgesMaxId(16, Array(13, 15))
)

val graph = ArrayBasedDirectedGraph(nodeSeqIterator, StoredGraphDir.BothInOut,
  NeighborsSortingStrategy.LeaveUnsorted)

val startingNode = 10
val dfs = new DepthFirstTraverser(graph, GraphDir.OutDir, Seq(startingNode))
  with DiscoveryAndFinishTimeTracker
  with ParentTracker
  with PathLengthTracker
val dfsNodes = dfs.toList
val dfsNo = dfsNodes.map(_.id).zipWithIndex.toMap
val parents: List[(Node, Option[Int])] = dfsNodes.map(x => x -> dfs.parent(x.id))
val children: Map[Option[Int], List[Int]] = parents
  .map{ case (k, v) => (v, k)}
  .groupBy(_._1)
  .mapValues(x => x.map(_._2.id))

val root = children(None)

val low = mutable.Map[Int, Int]()
dfsNodes.reverseIterator.foreach {
  node =>
    val dfsN = dfsNo(node.id)
    val backEdgesNodes: Seq[Int] = node.outboundNodes().filter(id => dfsNo.get(id) match {
      case Some(num) if num < dfsN => true
      case _ => false
    })
    low += (node.id -> (List(dfsNo(node.id)) ++
      backEdgesNodes.map(id => dfsNo(id)) ++
      children.getOrElse(Some(node.id), List()).map(low)).min)
}

low


