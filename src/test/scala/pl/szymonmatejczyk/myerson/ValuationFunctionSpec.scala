package pl.szymonmatejczyk.myerson

import org.scalatest.{Matchers, WordSpec}

/**
 * Created by szymonmatejczyk on 29.04.15.
 */
class ValuationFunctionSpec extends WordSpec with Matchers {
  "Superadditive valuation" should {
    "be good" in {
      val v = RandomValuationsGenerator.superadditiveUniform(3, 3.0)
      println("superadditive\n" + v.v.mkString("\n"))
    }
  }

  "submodular function" should {
    "be submodular" in {
      val v = RandomValuationsGenerator.submodularUniform(3, 3.0)
      println("submodular\n" + v.v.mkString("\n"))
    }
  }

  "supermodular function" should {
    "be supermodular" in {
      val v = RandomValuationsGenerator.supermodularUniform(3, 3.0)
      println("supermodular\n" + v.v.mkString("\n"))
    }
  }
}
