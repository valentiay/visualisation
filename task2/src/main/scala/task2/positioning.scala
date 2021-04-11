package task2

object positioning {

  final case class Graph(
                          nodes: Set[String],
                          ingoing: Map[String, Vector[String]],
                          outgoing: Map[String, Vector[String]]
                        )

  sealed trait Node extends Product with Serializable {
    def id: String
  }

  object Node {

    final case class Real(id: String) extends Node

    final case class Dummy(id: String) extends Node

  }

  final case class Positioning(
                                nodes: Map[Node, (Int, Int)],
                                edges: Map[Node, List[Node]]
                              )

  def topologicalOrdering(graph: Graph): Map[String, Int] = {
    final case class OrderingState(
                                    ordering: Map[String, Int],
                                    ingoingPositions: Map[String, List[Int]]
                                  )

    val lexicalOrdering =
      new Ordering[List[Int]] {
        def compare(xs: List[Int], ys: List[Int]): Int =
          xs.zip(ys).collectFirst {
            case (x, y) if x < y => -1
            case (x, y) if x > y => 1
          }.getOrElse(
            if (xs.length == ys.length) 0
            else if (xs.length < ys.length) -1
            else 1
          )
      }
    val initialState =
      OrderingState(
        ordering = Map.empty,
        ingoingPositions = (graph.nodes -- graph.ingoing.keySet).map(node => node -> List.empty[Int]).toMap
      )
    (0 until graph.nodes.size).foldLeft(initialState) { (state, idx) =>
      val nextNode = state.ingoingPositions.minBy(_._2)(lexicalOrdering)._1
      OrderingState(
        ordering = state.ordering.updated(nextNode, idx),
        ingoingPositions =
          state.ingoingPositions.removed(nextNode) ++
            graph.outgoing.getOrElse(nextNode, Vector.empty)
              .filterNot(node => state.ordering.contains(node))
              .map(node => node -> (idx :: state.ingoingPositions.getOrElse(node, Nil))).toMap
      )
    }.ordering
  }

  def minimizeIntersections(graph: Graph, nodeLayers: Map[String, Int], layers: Map[Int, Vector[String]]): Positioning = {
    val edgesWithLayers =
      for {
        source <- graph.nodes.toList
        target <- graph.outgoing.getOrElse(source, Vector.empty)
        sourceLayer = nodeLayers(source)
        edgeSize = nodeLayers(source) - nodeLayers(target)
        edge <- edgeSize match {
          case 1 => (sourceLayer, Node.Real(source), Node.Real(target)) :: Nil
          case _ =>
            (sourceLayer, Node.Real(source), Node.Dummy(source + target + 0)) ::
              (1 until edgeSize - 1).map(idx => (sourceLayer - idx, Node.Dummy(source + target + (idx - 1)), Node.Dummy(source + target + idx))).toList :::
              (sourceLayer - edgeSize + 1, Node.Dummy(source + target + (edgeSize - 2)), Node.Real(target)) :: Nil
        }
      } yield edge

    val edges = edgesWithLayers.groupMap(_._2)(_._3)
    val sinkNodeLayers =
      (graph.nodes -- graph.outgoing.keySet)
        .groupMapReduce(nodeLayers)(node => Set(Node.Real(node)))(_ ++ _)
    val otherNodeLayers =
      edgesWithLayers.groupMapReduce(_._1)(edge => Set(edge._2))(_ ++ _)
    val realLayers =
      sinkNodeLayers ++
        otherNodeLayers.transform((layer, nodes) => nodes ++ sinkNodeLayers.getOrElse(layer, Set.empty))

    val positions =
      realLayers.flatMap { case (layer, nodes) => nodes.zipWithIndex.map { case (node, idx) => node -> (idx, layer) } }

    Positioning(positions, edges)
  }

  def coffmanGrahamPositioning(graph: Graph, width: Int): Positioning = {
    val ordering = topologicalOrdering(graph)
    final case class PositioningState(
                                       nodeLayers: Map[String, Int],
                                       layers: Map[Int, Vector[String]],
                                     )
    val initialState = PositioningState(Map.empty, Map.empty)
    val PositioningState(nodeLayers, layers) = (0 until graph.nodes.size).foldLeft(initialState) { (state, _) =>
      val positioned = state.nodeLayers.keySet
      val node =
        (graph.nodes -- positioned)
          .filter(node => graph.outgoing.get(node).forall(_.forall(positioned.contains)))
          .maxBy(ordering)
      val minLayer =
        graph.outgoing.get(node).map(_.maxBy(state.nodeLayers)).map(state.nodeLayers).map(_ + 1).getOrElse(0)
      val allowedLayers =
        state
          .layers
          .collect { case (layer, nodes) if layer >= minLayer && nodes.size < width => layer }
      val layer =
        allowedLayers
          .minOption
          .getOrElse(state.layers.keys.maxOption.map(_ + 1).getOrElse(minLayer))
      PositioningState(
        state.nodeLayers.updated(node, layer),
        state.layers.updatedWith(layer)(_.map(_.appended(node)).orElse(Some(Vector(node))))
      )
    }

    minimizeIntersections(graph, nodeLayers, layers)
  }

}
