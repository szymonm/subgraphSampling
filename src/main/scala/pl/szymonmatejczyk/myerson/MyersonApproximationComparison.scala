package pl.szymonmatejczyk.myerson

import com.twitter.cassovary.graph.{DirectedGraph, TestGraphs}
import pl.szymonmatejczyk.generation.{PreferentialAttachment, AdditionalTestGraphs}
import purecsv.safe._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object MyersonApproximationComparison {

  case class Result(method: String, graph: String, function: String, samples: Int, error: Double)

  def mapDiff(m1: collection.Map[Int, Double], m2: collection.Map[Int, Double],
              errorFunction: (Double => Double)): Double = {
    require(m1.size == m2.size, "Map sizes do not fit")
    m1.iterator.map{ case (k, v) => errorFunction( math.abs( v - m2(k) ))}.sum
  }

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0).toDouble / 1000000000.0 + "s")
    result
  }

  def compare(graph: DirectedGraph, method: PowerIndexComputation,
              reference: collection.Map[Int, Double]) = {
    val res = method.apply()
    (mapDiff(res, reference, identity[Double]), res)
  }

  def compareSamplings(graph: DirectedGraph, method: IterativePowerIndexComputation,
                       samplings: Seq[Int],
                       reference: collection.Map[Int, Double]): Seq[Double] = {
    val res = method.apply(samplings)
    res.map (m => mapDiff(m, reference, identity[Double]))
  }

  def main(args: Array[String]): Unit = {
    if (args.length > 0)
      compute(args(0).toInt)
    else
      compute(7)
  }

  def socc(g: DirectedGraph, v: ValuationFunction) = new SumOfOverConnectedComponentsValuation(g, v)

  def compute(agents: Int): Seq[Result] = {
    val rand = new Random
    //    val graphs = List(TestGraphs.generateRandomUndirectedGraph(agents, 0.4))
    val graphs = List(
      (AdditionalTestGraphs.mutualCycle(agents), "cycle"),
      (TestGraphs.generateRandomUndirectedGraph(agents, 0.4), "ER(0.4)"),
      (new PreferentialAttachment(agents, 4, 3, rand).apply(), s"PA($agents, 4, 3)")
    )

    val samplings = List(100, 200, 300, 500, 1000, 1500, 2000, 3000, 4000, 5000, 7000, 10000, 15000)

    def valuationFunctions(g : DirectedGraph) =
      List[MyersonValuation](socc(g, new SizeValuation), socc(g, RandomValuationsGenerator.uniform(g.nodeCount)),
        socc(g, RandomValuationsGenerator.superadditiveUniform(agents, 2.0)))

    def methods(g: DirectedGraph, v: MyersonValuation, s: Int) = List[IterativePowerIndexComputation](
      //      new ExactMyersonValue(g, v),
      new SamplingCoalitionsMyersonValue(g, v, s)
      ,new HybridCoalitionsMyersonValue(g, v, s, 2)
      //      new MyersonFromCCGenerator(g, RandomCoalitionGenerator(g, new PerfectSampling(g, rand), s), v),
      ,new SumCCBasedSamplingCoalitionsMyersonValue(g, v, s)
    )

    val results = ArrayBuffer[Result]()

    val avg = 3

    for {
      g <- graphs
      v <- valuationFunctions(g._1)
    } {
      val exact = ExactMyersonValue(g._1, v)()
      println("Exact computed")
      for {
//        s <- samplings
        method <- methods(g._1, v, 0)
      } {
//        val r1 = compare(g._1, method, exact)
//        println(method.getClass.getCanonicalName + ": " + r1._1)
//        println(r1._2)

        def mean(s : Array[Double]) = s.sum / s.size

        val resArray: Array[Array[Double]] = Array.fill(samplings.size, avg)(0.0)

        time {
          for (i <- 0 until avg) {
            compareSamplings(g._1, method, samplings, exact).zipWithIndex.foreach {
              case (a, index) => resArray(index)(i) = a
            }
          }
        }

        samplings.zip(resArray.map(mean)).foreach {
          case (iterations, error) =>
            println(s"${method.getClass.getSimpleName}\t${g._2}\t${v.name}\t$iterations\t$error")
            results += Result(method.getClass.getSimpleName, g._2, v.name, iterations, error)
        }
      }
      results.toSeq.writeCSVToFileName(s"out-${g._2}-${v.name}.csv", sep = ",",
        header = Some(Seq("method", "graph", "function", "samples", "error")))
      results.clear()
    }

    results
  }
}
