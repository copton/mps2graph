package backend
package graphviz

object Printer {
  private def printInheritance(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      father <- node.fathers
    } yield node.name + " -> " + father + ";"

  private def printNodes(nodes: Seq[ast.Node]): Seq[String] =
    nodes.map(_.name + ";")

def print(nodes: Seq[ast.Node]): String =
    (
      List("digraph structure {") ++ 
      printNodes(nodes) ++
      printInheritance(nodes) ++
      List("}", "")
    ).mkString("\n")
}
