package filter

trait Filter {
  def apply(nodes: Seq[ast.Node]): Seq[ast.Node]
}

object RemoveBaseConcept extends Filter {
  def apply(nodes: Seq[ast.Node]): Seq[ast.Node] =
    nodes map filterNode

  private def filterNode(node: ast.Node): ast.Node =
    ast.Node(node.typ, node.name, node.children, node.references, node.fathers.filter(_ != "BaseConcept"), node.properties) 
}
