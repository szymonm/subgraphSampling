package pl.szymonmatejczyk.generation

import com.twitter.cassovary.graph.{ArrayBasedDirectedGraph, NeighborsSortingStrategy, NodeIdEdgesMaxId, StoredGraphDir}

object AdditionalTestGraphs {
  def g6mutual = ArrayBasedDirectedGraph(Seq(
    NodeIdEdgesMaxId(10, Array(11, 12, 15)),
    NodeIdEdgesMaxId(11, Array(10, 12, 15)),
    NodeIdEdgesMaxId(12, Array(10, 11, 13, 14)),
    NodeIdEdgesMaxId(13, Array(12)),
    NodeIdEdgesMaxId(14, Array(12)),
    NodeIdEdgesMaxId(15, Array(10, 11))
  ),
    StoredGraphDir.Mutual, NeighborsSortingStrategy.LeaveUnsorted)
}
