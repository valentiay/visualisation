import org.scalajs.dom.{CanvasRenderingContext2D, document}
import org.scalajs.dom.html.{Canvas, Input}

package object task1 {
  val showLabelsCheckbox: Input =
    document.getElementById("show-labels-checkbox").asInstanceOf[Input]
  val fileInput: Input =
    document.getElementById("file-selector").asInstanceOf[Input]

  val canvas: Canvas =
    document.getElementById("canvas").asInstanceOf[Canvas]
  val ctx: CanvasRenderingContext2D =
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
}
