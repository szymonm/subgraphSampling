package pl.szymonmatejczyk.subgraphsampling

import collection.mutable
import com.twitter.cassovary.graph._
import com.twitter.cassovary.graph.GraphDir.GraphDir
import com.twitter.cassovary.graph.tourist.IntInfoKeeper

class DFSConnectivityCheck(graph: DirectedGraph) {

  def articulationPoints(startingNode: Int, dir: GraphDir, onlyNodes: Int => Boolean = _ => true):
      collection.Set[Int] = {

    val dfs = new DepthFirstTraverser(graph, dir, Seq(startingNode))
      with DiscoveryAndFinishTimeTracker
      with ParentTracker
      with PathLengthTracker
      with NodesFilter {
      override def nodeFilter: (Int) => Boolean = onlyNodes
    }

    val dfsNodes = dfs.toList
    val dfsNo = dfsNodes.map(_.id).zipWithIndex.toMap
    val parents: List[(Node, Option[Int])] = dfsNodes.map(x => x -> dfs.parent(x.id))
    val parent = parents.toMap
    val children: Map[Option[Int], List[Int]] = parents
      .map{ case (k, v) => (v, k)}
      .groupBy(_._1)
      .mapValues(x => x.map(_._2.id))

    def backEdgeNodes(node: Node) = node.outboundNodes().filter(id => dfsNo.get(id) match {
      case Some(num) if num < dfsNo(node.id) && parent(node).map(_ != id).getOrElse(true) => true
      case _ => false
    })

    val low = mutable.Map[Int, Int]()
    dfsNodes.reverseIterator.foreach {
      node =>
        low += (node.id -> (List(dfsNo(node.id)) ++
          backEdgeNodes(node).map(id => dfsNo(id)) ++
          children.getOrElse(Some(node.id), List()).map(low)).min)
    }

    val root = children(None).head
    val rootArticulation = if (children(Some(root)).size > 1) List(root) else List[Int]()

    val innerAPs = low.filter(_._1 != root).filter{ case (id, lowValue) =>
      children.getOrElse(Some(id), List()).exists(x => low(x) > dfsNo(id))
    }.map(_._1)

    (rootArticulation ++ innerAPs).toSet
  }
}

trait ParentTracker extends DepthFirstTraverser {
  def rootIndicator = -1

  private lazy val nextVisitParent = new IntInfoKeeper(false)
  private lazy val parent = new IntInfoKeeper(true)

  abstract override protected def enqueue(nodes: Seq[Int], from: Option[Int]): Unit = {
    nodes foreach { id =>
      nextVisitParent.recordInfo(id, from.getOrElse(rootIndicator))
    }

    super.enqueue(nodes, from)
  }

  override def visitNode(node: Node) {
    parent.recordInfo(node.id, nextVisitParent.infoOfNode(node).get)
    super.visitNode(node)
  }

  def parent(id: Int): Option[Int] = parent.infoOfNode(id) match {
    case None => None
    case Some(r) if r == rootIndicator => None
    case parent => parent
  }
}

trait NodesFilter extends QueueBasedTraverser {
  def nodeFilter: Int => Boolean

  override protected def enqueue(nodes: Seq[Int], from: Option[Int]): Unit =
    super.enqueue(nodes.filter(nodeFilter), from)
}
