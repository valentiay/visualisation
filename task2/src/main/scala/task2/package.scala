import org.scalajs.dom.{CanvasRenderingContext2D, document}
import org.scalajs.dom.html.{Canvas, Input}
import org.scalajs.dom.raw.Element

package object task2 {
  val fileInput: Input =
    document.getElementById("file-selector").asInstanceOf[Input]
  val widthInput: Input =
    document.getElementById("width-selector").asInstanceOf[Input]
  val showLabelsCheckbox: Input =
    document.getElementById("show-labels-checkbox").asInstanceOf[Input]
  val showDummyLabelsCheckbox: Input =
    document.getElementById("show-dummy-labels-checkbox").asInstanceOf[Input]
  val loadingMessage: Element =
    document.getElementById("loading-message")

  val canvas: Canvas =
    document.getElementById("canvas").asInstanceOf[Canvas]
  val ctx: CanvasRenderingContext2D =
    canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
}
