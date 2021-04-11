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

  def median(vector: Seq[Int]): Int =
    vector.sorted.drop(vector.length / 2).headOption.getOrElse(vector.head)

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

  def countIntersections(layer: Int, outgoing: Map[Node, List[Node]], layers: Map[Int, Vector[Node]]): Int = {
    val upperLayer = layers(layer + 1)
    val currentLayer = layers(layer)
    val lowerLayer = layers(layer - 1)

    val upperEdges =
      upperLayer
        .flatMap(source => outgoing.getOrElse(source, Nil)
          .map(target => source -> target))

    val lowerEdges =
      currentLayer
        .flatMap(source => outgoing.getOrElse(source, Nil)
          .map(target => source -> target))

    val upperIntersections =
      (for {
        edge1@(source1, target1) <- upperEdges
        edge2@(source2, target2) <- upperEdges
        if edge1 != edge2 &&
          (upperLayer.indexOf(source1) - upperLayer.indexOf(source2)) *
            (currentLayer.indexOf(target1) - currentLayer.indexOf(target2)) < 0
      } yield (edge1, edge2)).size

    val lowerIntersections =
      (for {
        edge1@(source1, target1) <- lowerEdges
        edge2@(source2, target2) <- lowerEdges
        if edge1 != edge2 &&
          (currentLayer.indexOf(source1) - currentLayer.indexOf(source2)) *
            (lowerLayer.indexOf(target1) - lowerLayer.indexOf(target2)) < 0
      } yield (edge1, edge2)).size

    (upperIntersections + lowerIntersections) / 2
  }

  def locallyOptimize(layer: Int, outgoing: Map[Node, List[Node]], layers: Map[Int, Vector[Node]]): Map[Int, Vector[Node]] = {
    val nodes = layers(layer)
    (for {
      node1 <- nodes
      node2 <- nodes if node1 != node2
    } yield (node1, node2)).foldLeft(layers) { case (layers, (edge1, edge2)) =>
      val oldLayer = layers(layer)
      val newLayers =
        layers.updated(layer, oldLayer.updated(oldLayer.indexOf(edge1), edge2).updated(oldLayer.indexOf(edge2), edge1))
      if (countIntersections(layer, outgoing, newLayers) < countIntersections(layer, outgoing, layers)) {
        newLayers
      } else {
        layers
      }
    }
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

    val outgoing = edgesWithLayers.groupMap(_._2)(_._3)
    val ingoing = edgesWithLayers.groupMap(_._3)(_._2)

    val sinkNodeLayers =
      (graph.nodes -- graph.outgoing.keySet)
        .groupMapReduce(nodeLayers)(node => Set(Node.Real(node)))(_ ++ _)
    val otherNodeLayers =
      edgesWithLayers.groupMapReduce(_._1)(edge => Set(edge._2))(_ ++ _)
    val realLayers =
      sinkNodeLayers ++
        otherNodeLayers.transform((layer, nodes) => nodes ++ sinkNodeLayers.getOrElse(layer, Set.empty))
    val heuristicLayers =
      (1 to realLayers.keys.max).foldLeft[Map[Int, Vector[Node]]](Map(0 -> realLayers(0).toVector)) { (layers, layer) =>
        val nodes =
          realLayers(layer)
            .toVector
            .sortBy { source =>
              outgoing
                .get(source)
                .fold(Int.MaxValue)(targets => median(targets.map(target => layers(layer - 1).indexOf(target))))
            }
        layers.updated(layer, nodes)
      }

    //    val optimizedLayers = (1 until layers.keys.max).foldLeft(heuristicLayers)((layers, idx) => locallyOptimize(idx, outgoing, layers))

    val optimizedLayers =
      LazyList
        .iterate((heuristicLayers, heuristicLayers)) {
          case (_, oldLayers) =>
            val midLayres =
              (1 until layers.keys.max)
                .foldLeft(oldLayers)((layers, idx) => locallyOptimize(idx, outgoing, layers))
            val newLayers =
              (layers.keys.max - 1 to 1 by -1)
                .foldLeft(midLayres)((layers, idx) => locallyOptimize(idx, outgoing, layers))
            (oldLayers, newLayers)
        }
        .tail
        .takeWhile { case (oldLayers, newLayers) => oldLayers != newLayers }
        .lastOption
        .fold(heuristicLayers)(_._2)

    val positions =
      optimizedLayers.flatMap { case (layer, nodes) => nodes.zipWithIndex.map { case (node, idx) => node -> (idx, layer) } }

    Positioning(positions, outgoing)
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
