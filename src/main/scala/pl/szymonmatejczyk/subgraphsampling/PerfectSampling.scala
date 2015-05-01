package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph.{DirectedGraph, GraphDir}

import scala.collection.Set
import scala.util.Random

/**
 * Created by szymonmatejczyk on 24.04.15.
 */
class PerfectSampling(val graph: DirectedGraph, val rand: Random = new Random) extends SubgraphSampling[Int] {
  val subgraphs: Array[Set[Int]] = {
    val nodes = graph.iterator.map(_.id).toSet
    val connectivityCheck = new SubgraphsStructure(graph)
    nodes.subsets()
      .filter(subset => connectivityCheck.isConnected(subset, GraphDir.OutDir)).toArray
  }

  override def randomSubgraph(numberOfVertices: Int): Set[Int] = {
    throw new Exception()
  }

  override def randomSubgraph(): Set[Int] = {
    subgraphs(rand.nextInt(subgraphs.length))
  }
}
