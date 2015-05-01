package pl.szymonmatejczyk

import org.scalatest.{WordSpec, Matchers}
import pl.szymonmatejczyk.generation.PreferentialAttachment

import scala.util.Random

/**
 * Created by szymonmatejczyk on 29.04.15.
 */
class PreferentialAttachmentSpec extends WordSpec with Matchers {
  "PreferentialAttachment" should {
    "generate complete graph when no second phase nodes" in {
      val p = new PreferentialAttachment(5, 5, 4)
      p.apply().foreach {
        node =>
          node.inboundCount shouldEqual (4)
          node.outboundCount shouldEqual (4)
      }
    }
    "generate correct graph" in {
      val p = new PreferentialAttachment(10, 2, 3, new Random(1L))
      val g = p.apply()
      g.nodeCount should be (10)
    }
  }
}
