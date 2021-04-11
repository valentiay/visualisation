package task1
// Ключевой файл с кодом, который рассчиывает позиции для вершин дерева.
// Остальные файлы:
//  - drawing - логика по отрисовке дерева на canvas
//  - main - логика, связанная с интерефейсом
//  - parsing - логика по чтению дерева из файла
object positioning {

  /**
   * Дерево.
   *
   * @param root  id корневой вершины.
   * @param edges ассоциативный массив родитель -> список детей.
   */
  final case class Tree(root: String, edges: Map[String, Vector[String]])

  /**
   * Выкладка дерева на плоскости по сетке. Увеличение x - движение влево, увеличение y - движение вниз.
   *
   * @param positions   ассоциативный массив id вершины -> координаты на сетке.
   * @param leftBorder  список минимальными x-координатами для каждого слоя дерева.
   * @param rightBorder список с максимальными x-координатами для каждого слоя дерева.
   */
  final case class Positions(positions: Map[String, (Int, Int)], leftBorder: List[Int], rightBorder: List[Int])

  /**
   * Рассчиывает позицию на сетке для каждой вершины поддерева с помощью алгоритма Layered Tree Draw
   *
   * @param tree        дерево.
   * @param currentNode текущая вершина, чтобы определять поддерево.
   * @return расположение вершин на сетке для поддерва.
   */
  def calculateNodePositions(tree: Tree)(currentNode: String): Positions = {
    // Ищем детей текущей вершины.
    tree.edges.get(currentNode) match {
      case None =>
        // Если детей нет, то в текущем поддереве одна вершина, ставим ее в (0, 0).
        Positions(Map(currentNode -> (0, 0)), List(0), List(0))
      case Some(nodes) =>
        // Если дети есть, для каждого из поддеревьев рекурсивно рассчитываем позиции на сетке.
        // Затем попарно объединяем выкладки всех поддеревьев. Второе и последующие поддеревья при этом смещаем так,
        // чтобы не было пересечений и больших разрывов между поддеревьями.
        val Positions(newPositions, newLeftBorder, newRightBorder) =
          nodes.map(calculateNodePositions(tree)).reduceLeft { (leftDrawing, rightDrawing) =>
            // Считаем, на сколько позиций по оси x нужно сдвинуть правую выкладку,
            // чтобы расстояние между крайними вершинами на каждом слое не было меньше двух.
            // distance может быть отрицательным, т.е. правое поддерево может быть сдвинуто влево.
            val distance =
              leftDrawing.rightBorder.zip(rightDrawing.leftBorder)
                .map { case (leftX, rightX) => leftX + 2 - rightX }
                .max
            // Рассчиываем новые границы правого поддерева
            val movedRightBorder = rightDrawing.rightBorder.map(_ + distance)
            val movedLeftBorder = rightDrawing.leftBorder.map(_ + distance)
            // Рассчиываем позиции вершин в объединенной выкладке.
            // Правое поддерево сдвигаем, левое оставляем на месте.
            val newPositions =
              leftDrawing.positions ++
                rightDrawing.positions.transform { case (_, (x, y)) => (x + distance, y) }
            // Рассчиываем новые границы выкладки слева и справа.
            // Учитываем, что поддеревья могут быть разной высоты.
            val newLeftBorder =
              leftDrawing.leftBorder ::: movedLeftBorder.drop(leftDrawing.leftBorder.length)
            val newRightBorder =
              movedRightBorder ::: leftDrawing.rightBorder.drop(movedRightBorder.length)
            // Т.к. distance может быть отрицательным, новая выкладка может иметь отрицательные x-координаты.
            // Если это так, сдвигаем всю выкладку вправо.
            val minX = newLeftBorder.min
            if (minX == 0) {
              Positions(newPositions, newLeftBorder, newRightBorder)
            } else {
              Positions(
                newPositions.transform { case (_, (x, y)) => (x - minX, y) },
                newLeftBorder.map(_ - minX),
                newRightBorder.map(_ - minX),
              )
            }
          }
        // Считаем x-координату текущей вершины, так, чтобы она находилась посередине второго слоя поддерева.
        val newX = (newLeftBorder.head + newRightBorder.head) / 2
        // Добавляем координаты вершины поддерева в выкладку и сдвигаем нижние слои вниз на 1.
        Positions(
          newPositions.transform { case (_, (x, y)) => (x, y + 1) } + (currentNode -> (newX, 0)),
          newX :: newLeftBorder,
          newX :: newRightBorder,
        )
    }
  }
}
