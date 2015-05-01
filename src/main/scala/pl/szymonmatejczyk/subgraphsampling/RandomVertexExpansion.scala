package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph.{DirectedGraph, Node}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random


class RandomVertexExpansion(val graph: DirectedGraph, val rand: Random = new Random) extends SubgraphSampling[Int] {

  case class Edge(source: Node, target: Node) {
    def nodes = Set(source, target)
  }

  private def edges(node: Node) = node.outboundNodes().map(t => Edge(node, graph.getNodeById(t).get))

  @tailrec
  private def randomEdge(forbiddenSet: collection.Set[Int]): (Node, Node) = {
    new GraphRandoms(graph).randomEdge() match {
      case (a, b) if forbiddenSet.contains(a.id) || forbiddenSet.contains(b.id) =>
        randomEdge(forbiddenSet)
      case (a, b) => (a, b)
    }
  }

  private def randomSubgraph(numberOfVertices: Int, subgraphNodes: mutable.Set[Int],
                             outEdges: ArrayBuffer[Edge]): collection.Set[Int] = {
    while (subgraphNodes.size < numberOfVertices) {
      if (outEdges.isEmpty) {
        val (randomSource, randomTarget) = randomEdge(subgraphNodes)
        subgraphNodes ++= Seq(randomSource.id, randomTarget.id)
        outEdges ++= edges(randomSource) ++= edges(randomTarget)
      }
      val randomIdx = rand.nextInt(outEdges.size)
      val adding = outEdges(randomIdx).target
      outEdges(randomIdx) = outEdges.last
      outEdges.reduceToSize(outEdges.size - 1)
      if (!subgraphNodes.contains(adding.id)) {
        subgraphNodes += adding.id
        outEdges ++= edges(adding).filterNot(e => subgraphNodes.contains(e.target.id))
      }
    }
    subgraphNodes
  }

  override def randomSubgraph(numberOfVertices: Int): collection.Set[Int] = {
    randomSubgraph(numberOfVertices, mutable.Set[Int](), ArrayBuffer[Edge]())
  }
}
