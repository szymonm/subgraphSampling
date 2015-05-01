package pl.szymonmatejczyk.myerson

import com.twitter.cassovary.graph.{DirectedGraph, GraphDir}
import pl.szymonmatejczyk.subgraphsampling.{SubgraphsStructure, Subgraph}

import scala.collection.mutable

case class ExactMyersonValue(graph: DirectedGraph, v: ValuationFunction) extends PowerIndexComputation {
  import ExactMyersonValue._

  def apply(): collection.Map[Int, Double] = {
    val nodes = graph.iterator.map(_.id).toSet
    val mv = mutable.Map[Int, Double]() ++ nodes.map(x => (x, 0.0))
    val connectivityCheck = new SubgraphsStructure(graph)
    nodes.subsets()
      .filter (subset => connectivityCheck.isConnected(subset, GraphDir.OutDir))
      .foreach {
        cc =>
          val size = cc.size
          val neighbors = Subgraph(graph, cc).neighbors() -- cc
          cc.foreach {
            nodeInCC => mv += ((nodeInCC, mv(nodeInCC) + alpha(size, neighbors.size) * v(cc)))
          }
          neighbors.foreach {
            neighbor => mv += ((neighbor, mv(neighbor) - beta(size, neighbors.size) * v(cc)))
          }
    }
    mv
  }
}

object ExactMyersonValue {
  def alpha(s: Int, ns: Int): Double = {
    val numerator: Set[Int] = 2.to(ns).toSet
    val denom: Set[Int] = s.to(s + ns).toSet
    val intersection = numerator.intersect(denom)
    ((numerator -- intersection).foldLeft(BigDecimal(1.0))(_ * _) /
      (denom -- intersection).foldLeft(BigDecimal(1.0))(_ * _)).toDouble
  }

  def beta(s: Int, ns: Int): Double = {
    val n: Set[Int] = 2.to(ns - 1).toSet
    val d: Set[Int] = (s + 1).to(ns + s).toSet
    val intsec = n.intersect(d)
    ((n -- intsec).foldLeft(BigDecimal(1.0))(_ * _) / (d -- intsec).foldLeft(BigDecimal(1.0))(_ * _)).toDouble
  }
}
