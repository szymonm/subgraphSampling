package pl.szymonmatejczyk.myerson

/**
 * Created by szymonmatejczyk on 21.04.15.
 */
trait PowerIndexComputation {
  def apply(): collection.Map[Int, Double]
}

trait IterativePowerIndexComputation extends PowerIndexComputation {
  def apply(sequentialIterations: collection.Seq[Int]): collection.Seq[collection.Map[Int, Double]]
}
