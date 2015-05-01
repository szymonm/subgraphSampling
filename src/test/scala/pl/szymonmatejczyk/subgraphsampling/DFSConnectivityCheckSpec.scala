package pl.szymonmatejczyk.subgraphsampling

import com.twitter.cassovary.graph._
import org.scalatest.{WordSpec, Matchers}
import pl.szymonmatejczyk.generation.AdditionalTestGraphs

class DFSConnectivityCheckSpec extends WordSpec with Matchers {
  "Parent tracker should work correctly" in {
    val graph = TestGraphs.g6
    val dfs = new DepthFirstTraverser(graph, GraphDir.OutDir, Seq(10))
      with ParentTracker

    dfs.take(10).toList

    dfs.parent(10) should be (None)
    dfs.parent(11) should be (Some(10))
    dfs.parent(12) should be (Some(11))
    dfs.parent(13) should be (Some(10))
    dfs.parent(14) should be (Some(12))
    dfs.parent(15) should be (Some(14))
  }


  "DFSConnectivityCheck" should {
    "find articulation points for mutual graphs" when {
      "2-nodes path provided" in {
        val graph = TestGraphs.g2_mutual
        val aPs = new DFSConnectivityCheck(graph).articulationPoints(1, GraphDir.OutDir)
        aPs should be (Set[Int]())
      }

      "3-nodes path provided" in {
        val graph = TestGraph(
          TestNode(1, List(2), List(2)),
          TestNode(2, List(1, 3), List(1, 3)),
          TestNode(3, List(2), List(2))
        )

        graph.foreach {
          node =>
            val aPs = new DFSConnectivityCheck(graph)
              .articulationPoints(node.id, GraphDir.OutDir)
            aPs should be (Set(2))
        }
      }

      "6-nodes graph provided" in {
        val graph = AdditionalTestGraphs.g6mutual

        graph.foreach {
          node =>
            val aPs = new DFSConnectivityCheck(graph)
              .articulationPoints(12, GraphDir.OutDir)
            aPs should be (Set(12))
        }
      }
      "using nodes filter" in {
        val graph = AdditionalTestGraphs.g6mutual
        graph.foreach {
          node =>
            val aPs = new DFSConnectivityCheck(graph)
              .articulationPoints(12, GraphDir.OutDir, _ != 10)
            aPs should be (Set(11, 12))
        }
      }
    }
  }
}
