package task1

import org.scalajs.dom.DOMParser
import task1.positioning.Tree

object parsing {
  def parseTreeFromGraphML(graphML: String): Tree = {
    val parser = new DOMParser
    val xmlDoc = parser.parseFromString(graphML, "text/xml")
    val xmlGraph = xmlDoc
      .getElementsByTagName("graphml").item(0)
      .getElementsByTagName("graph").item(0)
    val xmlNodes = xmlGraph.getElementsByTagName("node")
    val xmlEdges = xmlGraph.getElementsByTagName("edge")

    val edges = for (i <- 0 until xmlEdges.length) yield
      (xmlEdges.item(i).getAttribute("source"),
        xmlEdges.item(i).getAttribute("target"))
    val edgesMap = edges.groupMap(_._1)(_._2).transform((_, targets) => targets.toVector)

    val nodes = for (i <- 0 until xmlNodes.length) yield
      xmlNodes.item(i).getAttribute("id")
    val targets = Set.from(edges.map(_._2))
    val root = nodes.find(node => !targets.contains(node)).get

    Tree(root, edgesMap)
  }
}
