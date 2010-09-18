package backend
package graphviz

object Printer extends backend.Printer {
  private def printInheritance(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      father <- node.fathers
    } yield "\"%s\" -> \"%s\" [arrowhead=empty];".format(node.name, father)

  private def printChildren(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      child <- node.children
    } yield "\"%s\" -> \"%s\" [arrowhead=diamond, taillabel=\"%s\", label=\"%s\"];".format(
      node.name, child.target, child.cardinality, child.name)

  private def printNodes(nodes: Seq[ast.Node]): Seq[String] =
    nodes map ((n) => n.typ match {
      case ast.NodeType.InterfaceConcept => "\"%s\" [shape=ellipse];".format(n.name)
      case ast.NodeType.Concept => "\"%s\" [shape=record, label=\"{%s%s}\"];".format(
        n.name, n.name, 
          if (n.properties.size == 0) "" 
          else "|" + (n.properties map ((p) => "%s: %s".format(p.name, p.typ)) mkString "\\l"))
    })

  def print(nodes: Seq[ast.Node]): String =
    (
      List("digraph structure {") ++ 
      printNodes(nodes) ++
      printInheritance(nodes) ++
      printChildren(nodes) ++
      List("}", "")
    ).mkString("\n")
}
