package task2

import org.scalajs.dom.DOMParser
import task2.positioning.Graph

object parsing {
  def parseGraphFromGraphML(graphML: String): Graph = {
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

    val outgoing = edges.groupMap(_._1)(_._2).transform((_, targets) => targets.toVector)
    val ingoing = edges.groupMap(_._2)(_._1).transform((_, sources) => sources.toVector)
    val nodes = (for (i <- 0 until xmlNodes.length) yield xmlNodes.item(i).getAttribute("id")).toSet

    Graph(nodes, ingoing, outgoing)
  }
}
