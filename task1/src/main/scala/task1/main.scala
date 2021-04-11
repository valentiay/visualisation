package task1

import org.scalajs.dom.{FileReader, UIEvent}
import task1.drawing._
import task1.parsing._
import task1.positioning._

object main extends App {
  def redrawTree(uiEvent: UIEvent): Unit = {
    if (fileInput.files.length == 1) {
      val file = fileInput.files.item(0)
      val reader = new FileReader
      reader.readAsText(file)
      reader.onload = _ => {
        val graphML = reader.result.asInstanceOf[String]
        val tree = parseTreeFromGraphML(graphML)
        val drawing = calculateNodePositions(tree)(tree.root)
        drawTree(tree, drawing)
      }
    }
  }

  fileInput.addEventListener[UIEvent]("change", redrawTree)
  showLabelsCheckbox.addEventListener[UIEvent]("change", redrawTree)
}
