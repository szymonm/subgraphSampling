package pl.szymonmatejczyk.myerson

import com.twitter.cassovary.graph.{DirectedGraph, GraphDir}
import pl.szymonmatejczyk.subgraphsampling.{SubgraphsStructure, Subgraph}

import scala.collection
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.util.Random

/**
 * Created by szymonmatejczyk on 21.04.15.
 */
trait ValuationFunction {
  def apply(s: collection.Set[Int]): Double

  def name: String
}

trait MyersonValuation extends ValuationFunction {
  def graph: DirectedGraph

  val cc = new SubgraphsStructure(graph)

  def forCC(s: collection.Set[Int]): Double
}

class SumOfOverConnectedComponentsValuation(val graph: DirectedGraph, v: ValuationFunction) extends MyersonValuation {
  def name = "SOCC " + v.name

  override def apply(s: collection.Set[Int]): Double = {
    cc.components(s, GraphDir.OutDir).map(v.apply).sum
  }

  override def forCC(s: collection.Set[Int]): Double = {
    v(s)
  }
}

class SizeValuation extends ValuationFunction {
  def name = "size valuation"
  override def apply(s: collection.Set[Int]): Double = s.size
}

trait Squared extends ValuationFunction {
  abstract override def apply(s: collection.Set[Int]): Double = {
    val v = super.apply(s)
    v * v
  }
}

class NeighbourhoodSize(graph: DirectedGraph) extends ValuationFunction {
  def name = getClass.getCanonicalName
  override def apply(s: collection.Set[Int]): Double = {
    val sg = Subgraph(graph, s)
    sg.neighbors().size
  }
}

class BigConnectedPremium(graph: DirectedGraph) extends ValuationFunction {
  def name = getClass.getCanonicalName
  override def apply(s: collection.Set[Int]): Double = {
    val cc = new SubgraphsStructure(graph)
    if (cc.isConnected(s, GraphDir.OutDir)) {
      1.0 / (graph.nodeCount - s.size + 1)
    } else {
      0.0
    }
  }
}

class CompletelyDefinedValuation(val v: collection.Map[collection.Set[Int], Double], n: String) extends ValuationFunction {
  def name = n
  override def apply(s: collection.Set[Int]): Double = v(s)
}

object RandomValuationsGenerator {
  val rand = new Random()

  def gaussian(mi: Double, stdDev: Double) = mi + (rand.nextGaussian() * stdDev)

  def binomial(successProb: Double): Double = if (rand.nextDouble() > successProb) 1 else 0

  def unif(a: Double, b: Double) = a + rand.nextDouble() * (b - a)

  private def independentValuation(name: String, gen: (collection.Set[Int] => Double))(n: Int): ValuationFunction = {
    val m = mutable.Map[collection.Set[Int], Double]()
    (0 until n).toSet.subsets().foreach {
      s => m += ((s, gen(s)))
    }
    new CompletelyDefinedValuation(m, name)
  }

  def uniform(n: Int): ValuationFunction = independentValuation("uniform", x => rand.nextDouble() * x.size)(n)

  def normal(n: Int): ValuationFunction = independentValuation("normal", x => gaussian(10 * x.size, 0.1))(n)

  def modifiedUniform(n: Int): ValuationFunction = independentValuation("uniform+", x => rand.nextDouble() * 10 * x.size +
    binomial(0.2) * unif(0, 50))(n)

  def modifiedNormal(n: Int) = independentValuation("normal+", x => gaussian(10 * x.size, 0.01) + binomial(0.2) * unif(0, 50))(n)

  def superadditiveUniform(n: Int, maxSynergy: Double): CompletelyDefinedValuation = {
    val players = BitSet() ++ (0 until n)
    val v = mutable.Map[collection.Set[Int], Double]()
    v(BitSet()) = 0.0
    (1 to n).foreach {
      size => players.subsets(size).foreach {
        subset =>
          var min = 0.0
          subset.subsets().filter(x => x.size != 0 && x.size != subset.size).foreach {
            s =>
              min = math.max(min, v(s) + v(subset -- s))
          }
          v(subset) = unif(min, min + maxSynergy)
      }
    }
    new CompletelyDefinedValuation(v, "superadditiveUniform")
  }

  /** Monotone submodular **/
  def submodularUniform(n: Int, maxSingleton: Double): CompletelyDefinedValuation = {
    val players = BitSet() ++ (0 until n)
    val v = mutable.Map[collection.Set[Int], Double]()
    v(BitSet()) = 0.0
    (1 to n).foreach {
      size => players.subsets(size).foreach {
        subset =>
          // min for monotonicity
          var min = 0.0
          subset.subsets().filter(x => x.size != 0 && x.size != subset.size).foreach {
            s =>
              min = math.max(min, v(s))
          }

          // max for submodularity
          var max = maxSingleton * subset.size
          subset.foreach {
            deleted =>
              subset.filter(_ != deleted).foreach {
                other =>
                  max = math.min(max, v(subset - deleted) + v(subset - other) - v (subset - other - deleted))
              }
          }
          v(subset) = unif(min, max)
      }
    }
    new CompletelyDefinedValuation(v, "submodularUniform")
  }

  /** Supermodular **/
  def supermodularUniform(n: Int, maxSingle: Double): CompletelyDefinedValuation = {
    val players = BitSet() ++ (0 until n)
    val v = mutable.Map[collection.Set[Int], Double]()
    v(BitSet()) = 0.0
    (1 to n).foreach {
      size => players.subsets(size).foreach {
        subset =>
          var min = 0.0

          // min for submodularity
          subset.foreach {
            deleted =>
              subset.filter(_ != deleted).foreach {
                other =>
                  min = math.max(min, v(subset - deleted) + v(subset - other) - v (subset - other - deleted))
              }
          }
          v(subset) = unif(min, min + maxSingle)
      }
    }
    new CompletelyDefinedValuation(v, "supermodularUniform")
  }
}
