package backend
package graphviz_usage

import scala.collection.mutable.HashSet

object Printer extends backend.Printer {

  private def printChildren(node: ast.Node): Seq[String] =
    node.children map ((child) =>
      "\"%s\" -> \"%s\" [arrowhead=diamond, taillabel=\"%s\", label=\"%s\", samehead=\"%s_child\"];".format(
        node.name, child.target, child.cardinality, child.name, child.target
      )
    )

  private def printReferences(node: ast.Node): Seq[String] =
    node.references map ((reference) =>
      "\"%s\" -> \"%s\" [arrowhead=ediamond, taillabel=\"%s\", label=\"%s\", samehead=\"%s_reference\"];".format(
        node.name, reference.target, reference.cardinality, reference.name, reference.target
      )
    )

  def printNode(node: ast.Node): String = (
    List("\"%s\" [shape=rectangle];".format(node.name)) ++
    printChildren(node) ++
    printReferences(node)
    ).mkString("\n")

  def print(nodes: Seq[ast.Node]): String = {
    var interestingNodes = HashSet[String]()
    for { 
      node <- nodes
      association <- node.children ++ node.references
    } interestingNodes.add(association.target)

    val lines = for {
      node <- nodes
      if node.children.size != 0 || node.references.size != 0 || interestingNodes.contains(node.name)
    } yield printNode(node) 

    (
      List("digraph structure {") ++ 
      lines ++
      List("}", "")
    ).mkString("\n")
  }     
}
