package backend
package graphviz

object Printer extends backend.Printer {
  private def printInheritance(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      father <- node.fathers
    } yield "\"%s\" -> \"%s\" [arrowhead=empty,samehead=\"%s_father\"];".format(node.name, father, father)

  private def printChildren(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      child <- node.children
    } yield "\"%s\" -> \"%s\" [arrowhead=diamond, taillabel=\"%s\", label=\"%s\", samehead=\"%s_child\"];".format(
      node.name, child.target, child.cardinality, child.name, child.target)

  private def printReferences(nodes: Seq[ast.Node]): Seq[String] =
    for {
      node <- nodes
      child <- node.references
    } yield "\"%s\" -> \"%s\" [arrowhead=ediamond, taillabel=\"%s\", label=\"%s\", samehead=\"%s_reference\"];".format(
      node.name, child.target, child.cardinality, child.name, child.target)

  private def printNodes(nodes: Seq[ast.Node]): Seq[String] =
    nodes map ((n) => n.typ match {
      case ast.NodeType.InterfaceConcept => "\"%s\" [shape=ellipse];".format(n.name)
      case ast.NodeType.Concept => 
        if (n.properties.size == 0) 
          "\"%s\" [shape=box];".format(n.name)
        else 
          "\"%s\" [shape=record, label=\"{%s|%s}\"];".format(
            n.name, n.name, 
            (n.properties map ((p) => "%s: %s".format(p.name, p.typ))) mkString "\\l")
    })

  def print(nodes: Seq[ast.Node]): String =
    (
      List("digraph structure {") ++ 
      printNodes(nodes) ++
      printInheritance(nodes) ++
      printChildren(nodes) ++
      printReferences(nodes) ++
      List("}", "")
    ).mkString("\n")
}
