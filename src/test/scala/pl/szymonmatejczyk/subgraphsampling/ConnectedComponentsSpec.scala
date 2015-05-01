package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph.GraphDir
import org.scalatest.{Matchers, WordSpec}
import pl.szymonmatejczyk.generation.AdditionalTestGraphs

/**
 * Created by szymonmatejczyk on 15.04.15.
 */
class ConnectedComponentsSpec extends WordSpec with Matchers {
  "ConnectedComponents" should {
    "be computed for the subgraph" in {
      val g = AdditionalTestGraphs.g6mutual
      val cc = new SubgraphsStructure(g)
      cc.components(Set(10, 11, 13, 15), GraphDir.OutDir) should be (Set(Set(10, 11, 15), Set(13)))
    }
  }
}
