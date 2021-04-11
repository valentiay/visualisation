package task1

import task1.positioning._

object drawing {
  val dx = 20
  val dy = 20
  val radius = 3

  def drawTree(tree: Tree, drawing: Positions): Unit = {
    val treeWidth = drawing.rightBorder.max - drawing.leftBorder.min
    val treeHeight = drawing.rightBorder.length.max(drawing.leftBorder.length)
    val scaleX = (canvas.width - 2 * dx) / treeWidth.max(1)
    val scaleY = (canvas.height - 2 * dy) / treeHeight.max(1)

    def transformCoordinates(coordinates: (Int, Int)): (Int, Int) =
      (coordinates._1 * scaleX + dx, coordinates._2 * scaleY + dy)

    ctx.clearRect(0, 0, canvas.width, canvas.height)
    drawing.positions.foreach {
      case (name, coordinates) =>
        ctx.beginPath()
        val (x, y) = transformCoordinates(coordinates)
        if (showLabelsCheckbox.checked) {
          ctx.fillText(name, x + radius + 1, y)
        }
        ctx.arc(x, y, radius, 0, 2 * Math.PI)
        ctx.fill()
    }

    tree.edges.foreach {
      case (source, targets) =>
        val (startX, startY) = transformCoordinates(drawing.positions(source))
        targets.foreach {
          target =>
            val (endX, endY) = transformCoordinates(drawing.positions(target))
            ctx.beginPath()
            ctx.moveTo(startX, startY)
            val middleY = (endY + startY) / 2
            ctx.bezierCurveTo(startX, middleY, endX, middleY, endX, endY)
            ctx.stroke()
        }
    }
  }
}
