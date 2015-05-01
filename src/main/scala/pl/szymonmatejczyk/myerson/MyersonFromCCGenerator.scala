package pl.szymonmatejczyk.myerson

import com.twitter.cassovary.graph.DirectedGraph
import pl.szymonmatejczyk.subgraphsampling.{SubgraphSampling, Subgraph}

import scala.collection.mutable
import scala.util.Random

/**
 * Created by szymonmatejczyk on 21.04.15.
 */
class MyersonFromCCGenerator(graph: DirectedGraph, generator: Generator[Subgraph], v: ValuationFunction)
  extends PowerIndexComputation{
  def apply(): collection.Map[Int, Double] = {
    val nodes = graph.iterator.map(_.id).toSeq
    val mv = mutable.Map() ++ nodes.map(x => (x, 0.0))
    var samples = 0
    StreamFromGenerator.apply[Subgraph](generator).iterator.foreach {
      subgraph =>
        samples += 1
        val size = subgraph.size
        val nodes = subgraph.nodes
        val neighbors = subgraph.neighbors() -- nodes
        subgraph.nodes.foreach {
          nodeInCC => mv += ((nodeInCC, mv(nodeInCC) + ExactMyersonValue.alpha(size, neighbors.size) * v(nodes)))
        }
        neighbors.foreach {
          neighbor => mv += ((neighbor, mv(neighbor) - ExactMyersonValue.beta(size, neighbors.size) * v(nodes)))
        }
    }
    mv.mapValues(x => x * math.pow(2, nodes.size - 1) / samples)
  }
}

trait Generator[T] {
  def generate(): Option[T]
}

trait Limit[T] extends Generator[T] {
  val limit: Int
  private var cur = 0
  
  abstract override def generate(): Option[T] = {
    cur += 1
    if (cur < limit) {
      super.generate()
    } else {
      None
    }
  }
  
}

object StreamFromGenerator {
  def apply[T](g: Generator[T]): Stream[T] = {
    g.generate() match {
      case Some(e) => Stream.cons(e, apply(g))
      case None => Stream.empty
    }
  }
}

class RandomCoalitionGenerator(graph: DirectedGraph, ss: SubgraphSampling[Int])
  extends Generator[Subgraph] {
  val r = new Random

  override def generate(): Option[Subgraph] = {
    Some(Subgraph(graph, ss.randomSubgraph()))
  }
}

object RandomCoalitionGenerator {
  def apply(graph: DirectedGraph, ss: SubgraphSampling[Int], steps: Int) = {
    new RandomCoalitionGenerator(graph, ss) with Limit[Subgraph] {
      override val limit: Int = steps
    }
  }
}
