package backend
package text

object Printer extends backend.Printer {
  private def printNode(node: ast.Node): String = node.toString

  def print(nodes: Seq[ast.Node]): String =
    (nodes map printNode).mkString("\n")
}
