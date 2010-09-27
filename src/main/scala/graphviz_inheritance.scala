package backend
package graphviz_inheritance

object Printer extends backend.Printer {
  private def printInheritance(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      father <- node.fathers
    } yield "\"%s\" -> \"%s\" [arrowhead=empty,samehead=\"%s_father\"];".format(node.name, father.getName, father.getName)

  private def printProperties(node: ast.Node): Seq[String] =
    node.properties map ((p) => "%s: %s".format(p.name, p.typ))

  private def printReferences(node: ast.Node): Seq[String] =
    node.references.map ((r) => "%s: *%s (%s)".format(r.name, r.target.getName, r.cardinality))

  private def printChildren(node: ast.Node): Seq[String] =
    node.children.map ((r) => "%s: %s (%s)".format(r.name, r.target.getName, r.cardinality))

  private def printNode(node: ast.Node): String = {
    val members = (
        printProperties(node) ++
        printChildren(node) ++
        printReferences(node))

    if (members.size == 0) {
      "\"%s\" [shape=%s];".format(
        node.name, node.typ match {
          case ast.NodeType.Concept => "rectangle"
          case ast.NodeType.InterfaceConcept => "ellipse"
        }
      )
    } else {
      "\"%s\" [shape=%s, label=\"{%s|%s}\"];".format(
        node.name, node.typ match {
          case ast.NodeType.Concept => "record"
          case ast.NodeType.InterfaceConcept => "Mrecord"
        }, node.name, members mkString "\\l"
      ) 
    }
  }

  private def printNodes(nodes: Seq[ast.Node]): Seq[String] = 
    nodes map printNode
  
  def print(nodes: Seq[ast.Node]): String =
    (
      List("digraph structure {", "concentrate=true;") ++ 
      printNodes(nodes) ++
      printInheritance(nodes) ++
      List("}", "")
    ).mkString("\n")
}
