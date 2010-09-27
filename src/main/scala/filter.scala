package filter

trait Filter {
  def apply(nodes: Seq[ast.Node]): Seq[ast.Node]
}

object UnresolvedTargets extends Filter {
  def apply(nodes: Seq[ast.Node]): Seq[ast.Node] =
    nodes map filterNode

  private def filterNode(node: ast.Node): ast.Node =
    ast.Node(node.typ, node.name, node.id, 
      node.children.filter(_.target.isResolved),
      node.references.filter(_.target.isResolved),
      node.fathers.filter(_.isResolved),
      node.properties
    )
}
