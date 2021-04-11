package task2

import task2.positioning.{Node, Positioning}

object drawing {
  val dx = 20
  val dy = 20
  val radius = 3

  def drawGraph(positioning: Positioning): Unit = {
    val graphWidth = positioning.nodes.values.maxBy(_._1)._1
    val graphHeight = positioning.nodes.values.maxBy(_._2)._2
    val scaleX = (canvas.width - 2 * dx) / graphWidth.max(1)
    val scaleY = (canvas.height - 2 * dy) / graphHeight.max(1)

    def transformCoordinates(coordinates: (Int, Int)): (Int, Int) =
      (coordinates._1 * scaleX + dx, coordinates._2 * scaleY + dy)

    ctx.clearRect(0, 0, canvas.width, canvas.height)
    positioning.nodes.foreach {
      case (node, coordinates) =>
        ctx.beginPath()
        val (x, y) = transformCoordinates(coordinates)
        node match {
          case Node.Real(id) =>
            if (showLabelsCheckbox.checked) ctx.fillText(id, x + radius + 1, y)
            ctx.arc(x, y, radius, 0, 2 * Math.PI)
          case Node.Dummy(id) =>
            if (showDummyLabelsCheckbox.checked) ctx.fillText(id, x + radius + 1, y)
            ctx.arc(x, y, radius / 2, 0, 2 * Math.PI)
        }
        ctx.fill()
    }

    positioning.edges.foreach {
      case (source, targets) =>
        val (startX, startY) = transformCoordinates(positioning.nodes(source))
        targets.foreach {
          target =>
            val (endX, endY) = transformCoordinates(positioning.nodes(target))
            ctx.beginPath()
            ctx.moveTo(startX, startY)
//            val middleY = (endY + startY) / 2
//            ctx.bezierCurveTo(startX, middleY, endX, middleY, endX, endY)
            ctx.lineTo(endX, endY)
            ctx.stroke()
        }
    }
  }
}
