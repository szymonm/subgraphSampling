package pl.szymonmatejczyk.generation

import com.twitter.cassovary.graph._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.util.Random

/**
 * Created by szymonmatejczyk on 29.04.15.
 */
class PreferentialAttachment(numberOfNodes: Int, initialCliqueSize: Int, neighbors: Int, rand: Random = new Random()) {
  def apply(): DirectedGraph = {
    require(initialCliqueSize > 0, "There must be at least one edge in the beginning")
    require(numberOfNodes >= initialCliqueSize, "Total number of nodes should be bigger than initial clique size")

    val adjacencyList = Array.tabulate[ArrayBuffer[Int]](numberOfNodes)(n =>
      if (n < initialCliqueSize) ArrayBuffer[Int]() ++ 0.until(n) ++ (n + 1 until initialCliqueSize)
      else ArrayBuffer[Int]()
    )

    val randomNodesArray = ArrayBuffer[Int]() ++
      (0 until initialCliqueSize).flatMap(x => Iterator.fill[Int](initialCliqueSize - 1)(x))

    def randomNeighbor(): Int = {
      randomNodesArray(rand.nextInt(randomNodesArray.length))
    }

    (initialCliqueSize until numberOfNodes).foreach {
      n =>
        val neighborsSet = mutable.Set[Int]()
        (0 until neighbors).map (
          x => neighborsSet += randomNeighbor()
        )
        neighborsSet.foreach {
          neigh =>
            randomNodesArray += neigh
            randomNodesArray += n
            adjacencyList(neigh) += n
        }
        adjacencyList(n) ++= neighborsSet
    }

    val nodes: Iterator[NodeIdEdgesMaxId] = adjacencyList.iterator
      .zipWithIndex.map{ case (el, ind) => NodeIdEdgesMaxId(ind, el.toArray)}

    ArrayBasedDirectedGraph(nodes.toIterable, StoredGraphDir.BothInOut, NeighborsSortingStrategy.LeaveUnsorted);
  }
}
