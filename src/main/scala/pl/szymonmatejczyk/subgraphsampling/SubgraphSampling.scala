package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph.DirectedGraph

import scala.util.Random

/**
 * Created by szymonmatejczyk on 04.03.15.
 */
trait SubgraphSampling[NodeId] {
  val graph: DirectedGraph
  val rand: Random

  def randomSubgraph(numberOfVertices: Int): collection.Set[NodeId]

  def randomSubgraph():collection.Set[NodeId] = {
    randomSubgraph(rand.nextInt(graph.nodeCount))
  }
}
