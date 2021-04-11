package task2

object positioning {

  /**
   * Представление графа.
   *
   * @param nodes    множество вершин.
   * @param ingoing  ассоциативный массив <id вершины> -> <массив id вершин, из которых есть ребра>
   * @param outgoing ассоциативный массив <id вершины> -> <массив id вершин, в которые есть ребра>
   */
  final case class Graph(
                          nodes: Set[String],
                          ingoing: Map[String, Vector[String]],
                          outgoing: Map[String, Vector[String]]
                        )

  /**
   * Представление вершины для отрисовки. Либо настоящая, либо dummy.
   */
  sealed trait Node extends Product with Serializable

  object Node {
    final case class Real(id: String) extends Node
    final case class Dummy(id: String) extends Node
  }

  /**
   * Расположение вершин на плоскости. Используются как настоящие, так и dummy-вершина.
   * Ребра есть только между соседними слоями.
   *
   * @param nodes ассоциативный массив <вершина> -> <координаты>
   * @param edges ассоциативный массив ребер <вершина> -> <список всех вершин, в которые есть рёбра>
   */
  final case class Positioning(nodes: Map[Node, (Int, Int)], edges: Map[Node, List[Node]])

  def median(vector: Seq[Int]): Int =
    vector.sorted.drop(vector.length / 2).headOption.getOrElse(vector.head)

  /**
   * Генерирует dummy-вершины, используя распределение по слоям.
   *
   * @param graph      входной граф.
   * @param nodeLayers распределение вершин по слоям.
   * @return Пару ассоциативный массив рёбер <id вершины> -> <список вершин, в которые есть ребро>, массив слоёв.
   *         В массиве рёбер есть только ребра между соседними слоями.
   */
  def addDummyNodes(graph: Graph, nodeLayers: Map[String, Int]): (Map[Node, List[Node]], Vector[Vector[Node]]) = {
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

    val sinkNodeLayers =
      (graph.nodes -- graph.outgoing.keySet)
        .groupMapReduce(nodeLayers)(node => Set(Node.Real(node)))(_ ++ _)
    val otherNodeLayers =
      edgesWithLayers.groupMapReduce(_._1)(edge => Set(edge._2))(_ ++ _)
    val realLayers =
      sinkNodeLayers ++
        otherNodeLayers.transform((layer, nodes) => nodes ++ sinkNodeLayers.getOrElse(layer, Set.empty))
    val layers = realLayers.toVector.sortBy(_._1).map(_._2.toVector)

    val edges = edgesWithLayers.groupMap(_._2)(_._3)

    (edges, layers)
  }

  /**
   * Эвристически оптимизирует число пересечений, пытаясь расположить вершины на следующем слое так,
   * чтобы каждая вершина находилась на медиане позиций потомков.
   *
   * @param edges  ассоциативный массив рёбер.
   * @param layers входной массив слоёв.
   * @return новый массив слоёв.
   */
  def heuristicallyOptimizeIntersections(edges: Map[Node, List[Node]], layers: Vector[Vector[Node]]): Vector[Vector[Node]] = {
    // Заполняем новый массив слоёв с начала по одному слою
    layers.foldLeft(Vector(layers.head)) { (layers, layer) =>
      val previousLayer = layers.last
      // Для каждой вершины в текущем слое считаем медиану позиций потомков,
      // и упорядочиваем вершины в текущем слое по медианам.
      // Если потомков нет, пытаемся расположить вершину в конце слоя.
      val currentLayer =
      layer
        .sortBy { source =>
          edges
            .get(source)
            .fold(Int.MaxValue)(targets => median(targets.map(previousLayer.indexOf)))
        }
      layers :+ currentLayer
    }
  }

  def countIntersections(upperLayerIdx: Int, edges: Map[Node, List[Node]], layers: Vector[Vector[Node]]): Int = {
    val upperLayer = layers(upperLayerIdx)
    val currentLayer = layers(upperLayerIdx - 1)

    val upperEdges =
      upperLayer
        .flatMap(source => edges.getOrElse(source, Nil)
          .map(target => source -> target))

    val upperIntersections =
      (for {
        edge1@(source1, target1) <- upperEdges
        edge2@(source2, target2) <- upperEdges
        if edge1 != edge2 &&
          (upperLayer.indexOf(source1) - upperLayer.indexOf(source2)) *
            (currentLayer.indexOf(target1) - currentLayer.indexOf(target2)) < 0
      } yield (edge1, edge2)).size

    upperIntersections / 2
  }

  /**
   * Локально оптимизирует пересечения ребер вверху и внизу слоя.
   *
   * @param layerIdx индекс слоя, который оптимизируется.
   * @param edges    ассоциативный массив рёбер.
   * @param layers   входной массив слоёв.
   * @return новый массив слоёв.
   */
  def locallyOptimizeIntersections(layerIdx: Int, edges: Map[Node, List[Node]], layers: Vector[Vector[Node]]): Vector[Vector[Node]] = {
    val layer = layers(layerIdx)
    // Попарно берем различные ноды и слоя.
    (for {
      node1 <- layer
      node2 <- layer if node1 != node2
    } yield (node1, node2)).foldLeft(layers) { case (layers, (edge1, edge2)) =>
      val oldLayer = layers(layerIdx)
      // Меняем местами вершины в слое.
      val newLayers =
        layers.updated(layerIdx, oldLayer.updated(oldLayer.indexOf(edge1), edge2).updated(oldLayer.indexOf(edge2), edge1))
      // Считаем число пересечений для исходного слоя.
      val oldIntersections =
        countIntersections(layerIdx + 1, edges, layers) + countIntersections(layerIdx, edges, layers)
      // Считаем число пересечений для слоя, в котором две вершины поменяны местами.
      val newIntersections =
        countIntersections(layerIdx + 1, edges, newLayers) + countIntersections(layerIdx, edges, newLayers)
      // Если пересечений стало меньше, изменяем слой.
      if (newIntersections < oldIntersections) newLayers else layers
    }
  }

  /**
   * Минимизирует число пересечений ребер в при отрисовке графа на плоскости.
   *
   * @param graph      входной граф.
   * @param nodeLayers распределение вершин по слоям.
   * @return представление графа на плоскости.
   */
  def minimizeIntersections(graph: Graph, nodeLayers: Map[String, Int]): Positioning = {
    // Генерируем dummy-ноды, в edges только ребра между соседними слоями.
    // edges - ассоциативный массив рёбер.
    // layers - массив слоёв.
    val (edges, layers) = addDummyNodes(graph, nodeLayers)
    // Сначала эвристически оптимизируем число пересечений, меняем порядок вершин внутри слоя.
    val a = System.nanoTime()
    val heuristicLayers = heuristicallyOptimizeIntersections(edges, layers)
    val b = System.nanoTime()
    // Локально оптимизируем число пересечений по слоям, до тех пор, пока это получается.
    val optimizedLayers =
      LazyList
        .iterate((heuristicLayers, heuristicLayers)) {
          case (_, oldLayers) =>
            // Проходим все слои, кроме первого и последнего от начала до конца и оптимизируем
            val midLayres =
              (1 until layers.size - 1)
                .foldLeft(oldLayers) { (layers, idx) => locallyOptimizeIntersections(idx, edges, layers) }
            // Проходим все слои, кроме первого и последнего от конца до начала и оптимизируем
            val newLayers =
              (layers.size - 2 to 1 by -1)
                .foldLeft(midLayres) { (layers, idx) => locallyOptimizeIntersections(idx, edges, layers) }
            (oldLayers, newLayers)
        }
        .tail
        .takeWhile { case (oldLayers, newLayers) => oldLayers != newLayers }
        .lastOption
        .fold(heuristicLayers)(_._2)

    // Каждой вершине присваиваем координату y - номер слоя, x - номер вершины в слое.
    val positions =
      optimizedLayers
        .reverse
        .zipWithIndex
        .flatMap { case (layer, y) => layer.zipWithIndex.map { case (node, x) => node -> (x, y) } }
        .toMap

    Positioning(positions, edges)
  }

  // Компаратор для упорядочивания кортежей индексов в лексикографическом порядке
  val lexicalOrdering =
    new Ordering[List[Int]] {
      def compare(xs: List[Int], ys: List[Int]): Int =
        xs.view.zip(ys.view).collectFirst {
          case (x, y) if x < y => -1
          case (x, y) if x > y => 1
        }.getOrElse(
          if (xs.length == ys.length) 0
          else if (xs.length < ys.length) -1
          else 1
        )
    }


  /**
   * Упорядочивает вершины в графе для алгоритма Грэма-Коффмана.
   *
   * @param graph входной граф.
   * @return ассоциативный массив <id вершины> -> <индекс при упорядочивании>.
   */
  def topologicalOrdering(graph: Graph): Map[String, Int] = {
    // Промежуточное состояние алгоритма упорядочивания.
    //   ordering - Ассоциативный массив <id вершины> -> <индекс при упорядочивании>.
    //              Среди ключей только уже распределенне вершины.
    //   ingoingPositions - Ассоциативный массив <id вершины> -> <кортеж индексов вершин, в которые исходят ребра>.
    //                      Среди ключей только нераспределенные вершины.
    final case class OrderingState(ordering: Map[String, Int], ingoingPositions: Map[String, List[Int]])
    // Начальное состояние: ни у одной вершины еще нет индекса.
    // В ingoingPositions помещаем вершины, у которых нет исходящих ребер.
    val initialState =
    OrderingState(
      ordering = Map.empty,
      ingoingPositions = (graph.nodes -- graph.ingoing.keySet).map(node => node -> List.empty[Int]).toMap
    )
    // Запускем итерацию алгоритма для каждого из возможных индексов, начина с нуля.
    (0 until graph.nodes.size).foldLeft(initialState) { (state, idx) =>
      // Находим вершину с минимальным кортежем индексов исходящий вершин.
      val nextNode = state.ingoingPositions.minBy(_._2)(lexicalOrdering)._1
      // Добавляем индекс текущей вершины в ассоциативный массив.
      val newOrdering = state.ordering.updated(nextNode, idx)
      // Удаляем из ассоциативного массива с кортежами только что распределенную вершину,
      // обновляем кортежи у еще не распределенных вершин.
      val newIngoingPositions =
      state.ingoingPositions.removed(nextNode) ++
        graph.outgoing.getOrElse(nextNode, Vector.empty)
          .filterNot(node => state.ordering.contains(node))
          .map(node => node -> (idx :: state.ingoingPositions.getOrElse(node, Nil)))
          .toMap

      OrderingState(newOrdering, newIngoingPositions)
    }.ordering
  }

  /**
   * Распределяет вершины графа по слоям, используя алгоритм Грэма-Коффмана.
   *
   * @param graph    входной граф.
   * @param ordering порядок вершин.
   * @param width    максимальное число вершин в слое.
   * @return ассоциативный массив <id вершины> -> <номер слоя>
   */
  def nodeLayersFromOrdering(graph: Graph, ordering: Map[String, Int], width: Int): Map[String, Int] = {
    // Промежуточное состояние алгоритма распределения вершин по слоям.
    //   nodeLayers - Ассоциативный массив <id вершины> -> <номер слоя>.
    //                В массиве только уже распределенные вершины.
    //   layers - Массив слоев. В массиве только уже распределенные вершины.
    final case class PositioningState(nodeLayers: Map[String, Int], layers: Vector[Vector[String]])
    // Изначально ни у одной вершины нет номера слоя.
    val initialState = PositioningState(Map.empty, Vector.empty)
    // Запускаем алгорит столько раз, сколько есть вершин в графе.
    (0 until graph.nodes.size).foldLeft(initialState) { (state, _) =>
      // Уже распредленные вершины
      val positioned = state.nodeLayers.keySet
      // Находим вершину с максимальным индексом, такую что все ее потомки уже распредлены.
      val node =
        (graph.nodes -- positioned)
          .filter(node => graph.outgoing.get(node).forall(_.forall(positioned.contains)))
          .maxBy(ordering)
      // Находим минимальный номер слоя, который больше всех номеров слоев потомков вершины.
      val minLayerIdx =
        graph.outgoing.getOrElse(node, Vector.empty).map(state.nodeLayers).maxOption.map(_ + 1).getOrElse(0)
      // Находим номер слоя, на который можно поместить вершину так, чтобы не нарушалось ограничение на ширину
      val layerIdx =
        state
          .layers
          .zipWithIndex
          .drop(minLayerIdx)
          .collectFirst { case (nodes, idx) if nodes.size < width => idx }
          .getOrElse(state.layers.size)
      // Добавляем вершину на слой, обновляем состояние
      val newLayers =
        if (layerIdx < state.layers.size) state.layers.updated(layerIdx, state.layers(layerIdx) :+ node)
        else state.layers :+ Vector(node)

      PositioningState(state.nodeLayers.updated(node, layerIdx), newLayers)
    }.nodeLayers
  }

  /**
   * Распределяет вершины графа на плоскости, используя алгоритм Грэма-Коффмана.
   *
   * @param graph входной граф.
   * @param width максимальная ширина.
   * @return расположение вершин графа на плоскости.
   */
  def coffmanGrahamPositioning(graph: Graph, width: Int): Positioning = {
    // Топологически упорядочиваем вершины графа
    val ordering = topologicalOrdering(graph)
    // Распределяем вершины по слоям, используя порядок
    val nodeLayers = nodeLayersFromOrdering(graph, ordering, width)
    // Минимизируем число пересечений
    minimizeIntersections(graph, nodeLayers)
  }

}
