package task2

import org.scalajs.dom.{FileReader, UIEvent}
import parsing.parseGraphFromGraphML
import positioning.coffmanGrahamPositioning
import drawing.drawGraph

object main extends App {
  def redrawGraph(e: UIEvent): Unit = {
    if (fileInput.files.length == 1) {
      loadingMessage.textContent = "Loading..."
      val file = fileInput.files.item(0)
      val reader = new FileReader
      reader.readAsText(file)
      reader.onload = _ => {
        val graphML = reader.result.asInstanceOf[String]
        val graph = parseGraphFromGraphML(graphML)
        val width = widthInput.value.toIntOption
        val positioning = coffmanGrahamPositioning(graph, width.getOrElse(Int.MaxValue))
        drawGraph(positioning)
        loadingMessage.textContent = ""
      }
    }
  }

  fileInput.addEventListener[UIEvent]("change", redrawGraph)
  widthInput.addEventListener[UIEvent]("change", redrawGraph)
  showLabelsCheckbox.addEventListener[UIEvent]("change", redrawGraph)
  showDummyLabelsCheckbox.addEventListener[UIEvent]("change", redrawGraph)
}
