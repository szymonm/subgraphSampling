package pl.szymonmatejczyk.myerson

import scala.io.Source
import scala.util.Random

/**
 * Created by szymonmatejczyk on 28.03.15.
 */
object MyersonValue extends App {
  val r = new Random()

  val coalitions = Source.fromFile("data/wtc-19-connected").getLines().map {
    line =>
      val (cc, aps) = line.split(" ").map(_.toInt).partition(_ > 0)
      (cc, aps.map(-_))
  }.toArray

  val n = coalitions.size

//  coalitions.foreach {
//    case (cc, aps) =>
//      println(cc.mkString(","))
//      println(aps.mkString(","))
//      println()
//  }

  val myerson = Array.fill[Double](20)(0.0)

  coalitions.foreach {
    case (cc, aps) =>
      aps.foreach {
        ap => myerson(ap) += 1
      }
  }

  (1 until 20).foreach {
    i => myerson(i) /= n
  }

  def random(): (Array[Int], Array[Int]) = {
    coalitions(r.nextInt(n))
  }

  def diff(a: Array[Double], b: Array[Double]): Double = {
    a.zip(b).map{ case (a, b) => math.abs(a - b)}.sum
  }

  val aprox = Array.fill[Double](20)(0.0)
  for (i <- 0 until n * n) {
    val (cc, aps) = random()
    aps.foreach {
      ap => aprox(ap) += 1
    }
    if (i % 100 == 0) println(i, diff(myerson, aprox.map(_ / i)))
  }

}
