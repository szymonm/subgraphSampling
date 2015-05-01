package pl.szymonmatejczyk.myerson

import com.twitter.cassovary.graph.DirectedGraph

import scala.collection.mutable

class PermutationBasedShapleyValue(graph: DirectedGraph, v: ValuationFunction)
  extends PowerIndexComputation {
  def apply(): collection.Map[Int, Double] = {
    val nodes = graph.iterator.map(_.id).toSet
    val mv = mutable.Map[Int, BigDecimal]() ++ nodes.map(x => (x, BigDecimal(0.0)))
    def addToMv(key: Int, value: Double): Unit = {
      mv += ((key, mv(key) + value))
    }
    nodes.toList.permutations.foreach {
      perm =>
        val s = mutable.Set[Int]()
        perm.foreach {
          el =>
            val without = v(s)
            s += el
            addToMv(el, v(s) - without)
        }
    }
    mv.mapValues(_ / factorial(nodes.size)).mapValues(_.toDouble)
  }
}
