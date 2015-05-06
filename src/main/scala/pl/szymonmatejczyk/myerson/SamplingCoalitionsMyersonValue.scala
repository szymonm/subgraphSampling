package pl.szymonmatejczyk.myerson

import com.twitter.cassovary.graph.DirectedGraph
import com.twitter.cassovary.util.Sampling
import pl.szymonmatejczyk.subgraphsampling.{Subgraph, SubgraphsStructure}

import scala.annotation.tailrec
import scala.collection._
import scala.util.Random

class SamplingCoalitionsMyersonValue(val graph: DirectedGraph, val v: ValuationFunction, val samples: Int,
                                     val rand: Random = new Random) extends IterativePowerIndexComputation {
  val nodesSet = BitSet() ++ graph.iterator.map(_.id)

  val nodesArray = nodesSet.toArray

  val chosen = 0
  val nodesButChosenArray = (nodesSet - chosen).toArray

  val N = nodesSet.size

  def sizeSample(max: Int) = {
    rand.nextInt(max + 1)
  }

  def finish(mv: collection.Map[Int, Double], samples: Int) = {
    mv.map { case (k, v) => (k, v / samples)}
  }

  def zerosMap(nodes: Set[Int]) = mutable.Map[Int, Double]() ++ nodes.map(n => (n, 0.0))

  protected def samplingBasedMyerson(samples: Int): collection.Map[Int, Double] = {
    val mv = zerosMap(nodesSet)

    updateMapBySampling(mv, samples)
    finish(mv, samples)
  }

  def apply(): collection.Map[Int, Double] = {
    samplingBasedMyerson(samples)
  }

  def updateMapBySampling(mv: mutable.Map[Int, Double], samples: Int): Unit = {
    (0 until samples).foreach {
      i =>
        val k = sizeSample(N - 1)
        val c = Sampling.randomSubset(k, nodesButChosenArray, rand).toSet
        nodesSet.foreach {
          n =>
            val cprime = if (n == chosen) c else if (c.contains(n)) c - n + chosen else c
            val mc = v(cprime + n) - v(cprime)
            mv(n) = mv(n) + mc
        }
    }
  }

  override def apply(sequentialIterations: collection.Seq[Int]): collection.Seq[Map[Int, Double]] = {
    val mv = zerosMap(nodesSet)

    var totalSamples = 0

    sequentialIterations.map {
      samples =>
        updateMapBySampling(mv, samples)
        totalSamples += samples
        finish(mv, totalSamples)
    }
  }
}

class HybridCoalitionsMyersonValue(override val graph: DirectedGraph, override val v: ValuationFunction, override val samples: Int,
                                   val exactCoalitionsMaxSize: Int, override val rand: Random = new Random)
  extends SamplingCoalitionsMyersonValue(graph, v, samples, rand) {
  require(2 * exactCoalitionsMaxSize < N)

  override def sizeSample(max: Int): Int = {
    val from = exactCoalitionsMaxSize + 1
    val to = max - exactCoalitionsMaxSize
    if (from == to)
      from
    else
      from + rand.nextInt(to - from)
  }

  def permToCoalitionFraction1(n : Int, v: Int) = (factorial(v) * factorial(n - v - 1) / factorial(n)).toDouble
  def permToCoalitionFraction2(n : Int, v: Int) = (factorial(v - 1) * factorial(n - v) / factorial(n)).toDouble

  def exactComputation(maxSize: Int): (collection.Map[Int, Double], Int) = {
    val N = graph.nodeCount

    val mv = zerosMap(nodesSet)
    var subsetsCount = 0

    (0 to maxSize).flatMap(x => nodesSet.subsets(x)).foreach {
      subset =>
        subsetsCount += 1
        val valC = v(subset)
        nodesSet.filter(x => !subset.contains(x)).foreach {
          n =>
            mv(n) = mv(n) + permToCoalitionFraction1(N, subset.size) * (v(subset + n) - valC)
        }

        val compl = nodesSet -- subset
        val valCompl = v(compl)
        nodesSet.filter(x => compl.contains(x)).foreach {
          n =>
            mv(n) = mv(n) + permToCoalitionFraction2(N, compl.size) * (valCompl - v(compl - n))
        }
    }
    (mv, subsetsCount)
  }

  def joinResults(mvExact: collection.Map[Int, Double], mvSampled: collection.Map[Int, Double]):
    collection.Map[Int, Double] = {
    mvSampled.map {
      case (k, value) => (k, mvExact(k) + value * (N - 2 * exactCoalitionsMaxSize - 2).toDouble / N)
    }
  }

  override def apply(): collection.Map[Int, Double] = {
    val (mv, exactCount) = exactComputation(exactCoalitionsMaxSize)

    val mvSampled: collection.Map[Int, Double] = samplingBasedMyerson(samples - exactCount)

    joinResults(mv, mvSampled)
  }

  override def apply(sequentialIterations: collection.Seq[Int]): collection.Seq[Map[Int, Double]] = {
    val (mvExact, exactCount) = exactComputation(exactCoalitionsMaxSize)

    var skipLeft = exactCount
    val (skipped, rest) = sequentialIterations.span(x => if (skipLeft > x) {skipLeft -= x; true} else false)

    val mvSampled = zerosMap(nodesSet)

    var totalSamples = 0

    val estimations: Seq[collection.Map[Int, Double]] = ((rest.head - skipLeft) +: rest.drop(1)).map {
      samples =>
        updateMapBySampling(mvSampled, samples)
        totalSamples += samples
        joinResults(mvExact, finish(mvSampled, totalSamples))
    }

    val zeros = zerosMap(nodesSet)
    skipped.map(_ => zeros) ++ estimations
  }
}

class SumCCBasedSamplingCoalitionsMyersonValue(val graph: DirectedGraph, val v: MyersonValuation,
                                               val samples: Int,
                                               val rand: Random = new Random) extends IterativePowerIndexComputation {
  val nodesSet = BitSet() ++ graph.iterator.map(_.id)

  val cc = new SubgraphsStructure(graph)

  @tailrec
  private def randomNonEmptySubset(set: collection.Set[Int]): collection.Set[Int] = {
    set.filter(x => rand.nextBoolean()) match {
      case s: Set[Int] if s.isEmpty => randomNonEmptySubset(set)
      case s => s
    }
  }

  def updateMapBySampling(mv: mutable.Map[Int, Double], samples: Int): Unit = {
    (0 until samples).foreach {
      i =>
        val c = randomNonEmptySubset(nodesSet)
        if (cc.isConnected(c)) {
          val neighbors = Subgraph(graph, c).neighbors()
          c.foreach {
            n =>
              mv(n) = mv(n) + ExactMyersonValue.alpha(c.size, neighbors.size) * v.forCC(c)
          }
          neighbors.foreach {
            n =>
              mv(n) = mv(n) - ExactMyersonValue.beta(c.size, neighbors.size) * v.forCC(c)
          }
        }
    }
  }

  def finish(mv: collection.Map[Int, Double], samples: Int) = {
    val multiplier = ((BigDecimal(2).pow(nodesSet.size) - 1) / samples).toDouble
    mv.mapValues(_  * multiplier)
  }

  override def apply(): collection.Map[Int, Double] = {
    val mv = mutable.Map[Int, Double]() ++ nodesSet.map(n => (n, 0.0))

    updateMapBySampling(mv, samples)
    finish(mv, samples)

  }

  override def apply(sequentialIterations: scala.Seq[Int]): scala.Seq[Map[Int, Double]] = {

    val mv = mutable.Map[Int, Double]() ++ nodesSet.map(n => (n, 0.0))

    var totalSamples = 0

    sequentialIterations.map {
      samples =>
        updateMapBySampling(mv, samples)
        totalSamples += samples
        finish(mv, totalSamples)
    }
  }
}

class TimeAlignedSumCCBasedSamplingCoalitionsMyersonValue(override val graph: DirectedGraph, override val v: MyersonValuation,
                                                          override val samples: Int,
                                                          override val rand: Random = new Random)
  extends SumCCBasedSamplingCoalitionsMyersonValue(graph, v, samples, rand) {

  override def apply(sequentialIterations: scala.Seq[Int]): scala.Seq[Map[Int, Double]] = super.apply(sequentialIterations.map(_ * 30))
}
