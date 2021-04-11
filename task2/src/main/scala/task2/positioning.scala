package task2

object positioning {

  final case class Graph(
                          nodes: Set[String],
                          ingoing: Map[String, Vector[String]],
                          outgoing: Map[String, Vector[String]]
                        )

}
