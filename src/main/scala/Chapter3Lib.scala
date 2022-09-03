import scala.collection.mutable
import scala.collection.mutable.ListBuffer
//////////  WEEK 3

object Week3Lib {

  import X86int._
  import WGraph._

  sealed abstract class W
  case class Id(x: String) extends W
  case class Rg(r: Register) extends W

  case class LiveBlockInfo(after: List[Set[W]]) extends BlockInfo

  case class ProgramInterferenceInfo(conflicts: G) extends Info

  case class ProgramRegisterInfo(stackSpace: Long, calleeRegsUsed: List[Register]) extends Info

}

trait GraphIntf {

  type NodeId
  type G

  def directedGraph(edges: List[(NodeId, NodeId)]): G
  def addNode(g: G, node: NodeId): G
  def addDirectedEdge(g: G, src: NodeId, tgt: NodeId): G
  def addUndirectedEdge(g: G, src: NodeId, tgt: NodeId): G = {
    addDirectedEdge(addDirectedEdge(g, src, tgt), tgt, src)
  }
  def neighbors(g: G, node: NodeId): Set[NodeId] = {
    inNeighbors(g, node) ++ outNeighbors(g, node)
  }
  def outNeighbors(g: G, node: NodeId): Set[NodeId]
  def inNeighbors(g: G, node: NodeId): Set[NodeId]
  def nodes(g: G): Set[NodeId]
  def toposort(g: G): List[NodeId]

  def toDotGraph(g: G): String = { // undirected
    "graph G {\n" +
      nodes(g).map({ n =>
        outNeighbors(g, n).map({ m =>
          "  \"" + n + "\" -- \"" + m + "\";\n" }).mkString("") }).mkString("") +
      "}"
  }

  def toDotDigraph(g: G): String = { // undirected
    "digraph G {\n" +
      nodes(g).map({ n =>
        outNeighbors(g, n).map({ m =>
          "  \"" + n + "\" -> \"" + m + "\";\n" }).mkString("") }).mkString("") +
      "}"
  }

}

class Graph extends GraphIntf {
  override type G =
    (
      Set[NodeId],           // set of all vertices in the graph
        NodeId => Set[NodeId], // out-neighbors
        NodeId => Set[NodeId]  // in-neighbors
      )

  def empty: G = (Set.empty, { n => Set.empty }, { n => Set.empty })

  override def directedGraph(edges: List[(NodeId, NodeId)]): G =
    (
      { val (n1s, n2s) = edges.unzip
        n1s.toSet ++ n2s.toSet },
      (n: NodeId) =>
        edges.filter({ case (m, _) => n == m }).map({ case (_, x) => x }).toSet,
      (n: NodeId) =>
        edges.filter({ case(_, m) => n == m }).map({ case(x, _) => x }).toSet
    )

  override def addNode(g: G, node: NodeId): G = g match {
    case (nodes, out, in) =>
      (
        nodes + node, out, in
      )
  }

  override def addDirectedEdge(g: G, src: NodeId, tgt: NodeId): G = g match {
    case (nodes, out, in) =>
      (
        nodes + src + tgt,
        { n => out(n) ++ (if (n == src) List(tgt).toSet else Nil) },
        { n => in(n)  ++ (if (n == tgt) List(src).toSet else Nil) }
      )
  }

  override def outNeighbors(g: G, node: NodeId): Set[NodeId] = g._2(node)
  override def inNeighbors(g: G, node: NodeId): Set[NodeId] = g._3(node)
  override def nodes(g: G): Set[NodeId] = g._1

  override def toposort(g: G): List[NodeId] = {
    val result: ListBuffer[NodeId] = ListBuffer()
    var result_set: Set[NodeId] = Set()
    val nodesToGo: ListBuffer[NodeId] = nodes(g).filter(n => outNeighbors(g, n).isEmpty).to

    while (nodesToGo.nonEmpty) {
      val node = nodesToGo.remove(0)
      result += node
      result_set = result_set + node
      inNeighbors(g, node).foreach{nb =>
        if (!result_set.contains(nb) && (outNeighbors(g, nb) -- result_set).isEmpty) {
          nodesToGo += nb
        }
      }
    }

    assert(result_set == nodes(g), "Graph was cyclic")

    result.toList
  }
}

object WGraph extends Graph {
  import Week3Lib._

  override type NodeId = W
}

object SGraph extends Graph {
  override type NodeId = String
}