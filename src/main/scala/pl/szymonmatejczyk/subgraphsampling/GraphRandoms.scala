package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph.{Node, DirectedGraph}

import scala.util.Random

class GraphRandoms(graph: DirectedGraph, rand: Random = new Random) {
  val nodes = graph.iterator.toArray

  def randomNode(): Node = {
    nodes(rand.nextInt(nodes.length))
  }

  def randomEdge(): (Node, Node) = {
    val edgeNo = rand.nextLong() % graph.edgeCount match {
      case x: Long if x < 0 => x + graph.edgeCount
      case x => x
    }
    var counter = edgeNo
    val source = graph.iterator.dropWhile(x => {counter -= x.outboundCount; counter >= 0}).next()
    (source, graph.getNodeById(source.outboundNodes()(source.outboundCount + counter.toInt)).get)
  }
}
