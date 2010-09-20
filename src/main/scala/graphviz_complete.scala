package backend
package graphviz_complete

object Printer extends backend.Printer {
  private def printInheritance(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      father <- node.fathers
    } yield "\"%s\" -> \"%s\" [arrowhead=none, arrowtail=dot, color=blue, samehead=\"%s_father\"];".format(node.name, father, father)

  private def printChildren(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      child <- node.children
    } yield "\"%s\" -> \"%s\" [arrowhead=none, arrowtail=dot, color=green, samehead=\"%s_child\"];".format(
      node.name, child.target, child.cardinality, child.name, child.target)

  private def printReferences(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      reference <- node.references
    } yield "\"%s\" -> \"%s\" [arrowhead=none, arrowtail=dot, color=green, samehead=\"%s_reference\"];".format(
      node.name, reference.target, reference.cardinality, reference.name, reference.target)

  private def printMemberProperties(node: ast.Node): Seq[String] =
    node.properties map ((p) => "%s: %s".format(p.name, p.typ))

  private def printMemberReferences(node: ast.Node): Seq[String] =
    node.references.map ((r) => "%s: *%s (%s)".format(r.name, r.target, r.cardinality))

  private def printMemberChildren(node: ast.Node): Seq[String] =
    node.children.map ((r) => "%s: %s (%s)".format(r.name, r.target, r.cardinality))

  private def printMemberFathers(node: ast.Node): Seq[String] = node.fathers

  private def printNode(node: ast.Node): String = {
    val members = (
        printMemberProperties(node) ++
        printMemberChildren(node) ++
        printMemberReferences(node))

    val fathers = printMemberFathers(node)

    "\"%s\" [shape=%s, label=\"{%s|%s|%s}\"];".format(
      node.name, node.typ match {
        case ast.NodeType.Concept => "record"
        case ast.NodeType.InterfaceConcept => "Mrecord"
      }, node.name, fathers mkString "\\n", members mkString "\\n"
    ) 
  }

  private def printNodes(nodes: Seq[ast.Node]): Seq[String] = 
    nodes map printNode

  def print(nodes: Seq[ast.Node]): String =
    (
      List("digraph correct {", "compact=true;") ++ 
      printNodes(nodes) ++
      printInheritance(nodes) ++
      printChildren(nodes) ++
      printReferences(nodes) ++
      List("}", "")
    ).mkString("\n")
}
