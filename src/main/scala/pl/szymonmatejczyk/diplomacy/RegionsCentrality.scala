package pl.szymonmatejczyk.diplomacy

import com.twitter.cassovary.graph.{DirectedGraph, Node}
import com.twitter.cassovary.util.SequentialNodeNumberer
import com.twitter.cassovary.util.io.AdjacencyListGraphReader

/**
 * Created by szymonmatejczyk on 29.04.15.
 */
object RegionsCentrality {
  def factorial(num: Int): BigDecimal = {
    if (num < 0)
      0
    else
      (1 to num).map(x => BigDecimal.valueOf(x)).foldLeft(BigDecimal.valueOf(1)) ((a,b) => a * b)
  }

  def partitions(k: Int, d: Int): List[List[Int]]= {
    if (d == 1) {
      List(List(k))
    } else {
      0.to(k).flatMap(a0 => partitions(k - a0, d - 1).map(a0 +: _)).toList
    }
  }

  def newton(k: Int, l: List[Int]): BigDecimal = {
    factorial(k) / l.map(factorial).foldLeft(BigDecimal(1))(_ * _)
  }

  def productFormula(l: List[Int]): Int = {
    l.zipWithIndex.map{ case (a, b) => a * b}.sum
  }

  def product(l : List[Int]): BigDecimal = {
    l.zipWithIndex.map{ case (a, b) => BigDecimal(1) / factorial(b).pow(a)}.foldLeft(BigDecimal(1))(_ * _)
  }


  def kappa(k: Int, d: Int, n: Int): BigDecimal = {
    if (n > k * d) {
      0
    } else {
      factorial(n) * partitions(k, d + 1).filter(x => productFormula(x) == n).map(x => newton(k, x) * product(x)).sum
    }
  }

  def alpha(n: Int, k: Int): BigDecimal = {
    (1 to n).map {
      l => factorial(n - l) * BigDecimal(k - 1).pow(n - l + 1) * (kappa(k - 1, n - l, l - 1) - kappa(k - 1, n - l - 1, l - 1))
    }.sum
  }

  def beta(n: Int, k: Int): BigDecimal = {
    def f = factorial _
    (1 to n)
      .filter(l => l - 1 - (n - l) >= 0)
      .map {
        l => f(n - l) * BigDecimal(k - 1).pow(n - l + 1) * f(l - 1) / f(l - 1 - (n - l)) * kappa(k - 2, n - l, l - 1 - (n - l))
      }.sum
  }

  def computeSkibskiValue(graph: DirectedGraph, node : Node, k: Int): BigDecimal = {
    node.outboundNodes().map(graph.getNodeById).map(_.get).map {
      n => (alpha(n.outboundCount, k) + beta(n.outboundCount, k)) / (factorial(n.outboundCount) * BigDecimal(k - 1).pow(n.outboundCount))
    }.sum
  }

  def main(args: Array[String]): Unit = {
    val nodeNumberer = new SequentialNodeNumberer[String]()
    val graph = new AdjacencyListGraphReader[String]("src/main/resources", "board", nodeNumberer,
      identity[String]).toArrayBasedDirectedGraph()
    println("Graph loaded")
    val k = 7

    val rank = graph.map {
      n =>
        println("Computing for: " + nodeNumberer.internalToExternal(n.id))
        (nodeNumberer.internalToExternal(n.id), computeSkibskiValue(graph, n, k))
    }.toList.sortBy(_._2).map{case (a, b) => f"$a:\t\t${b.toDouble}%2.3f"}.mkString("\n")

    println(rank)

  }
}
