package backend
package graphviz_usage

import scala.collection.mutable.HashSet

object Printer extends backend.Printer {

  private def printChildren(node: ast.Node): Seq[String] =
    node.children map ((child) =>
      "\"%s\":\"%s\" -> \"%s\" [arrowhead=diamond, samehead=\"%s_child\"];".format(
        node.name, child.name, child.target.getName, child.target.getName
      )
    )

  private def printReferences(node: ast.Node): Seq[String] =
    node.references map ((reference) =>
      "\"%s\":\"%s\" -> \"%s\" [arrowhead=ediamond, samehead=\"%s_reference\"];".format(
        node.name, reference.name, reference.target.getName, reference.target.getName
      )
    )

  def printChildMembers(children: Seq[ast.Association]): Seq[String] =
    children map ((c) =>
      "<%s>%s: %s (%s)".format(c.name, c.name, c.target.getName, c.cardinality))

  def printReferenceMembers(references: Seq[ast.Association]): Seq[String] =
    references map ((r) =>
      "<%s>%s: %s (%s)".format(r.name, r.name, r.target.getName, r.cardinality))

  def printNode(node: ast.Node): String = (
      List("\"%s\" [shape=%s,label=\"%s|%s\"];".format(
        node.name,
        node.typ match {
          case ast.NodeType.Concept => "record"
          case ast.NodeType.InterfaceConcept => "Mrecord"
        },
        node.name,
        (
          printChildMembers(node.children) ++ 
          printReferenceMembers(node.references) 
        ) mkString "|"
      )) ++
      printChildren(node) ++
      printReferences(node)
    ).mkString("\n")

  def print(nodes: Seq[ast.Node]): String = {
    val interestingNodes = new HashSet[String]() ++ (for { 
      node <- nodes
      association <- node.children ++ node.references
    } yield association.target.getName)

    val lines = for {
      node <- nodes
      if node.children.size != 0 || node.references.size != 0 || interestingNodes.contains(node.name)
    } yield printNode(node) 

    (
      List(
        "digraph structure {", 
        "concentrate=true;",
        "rankdir=LR;"
      ) ++ 
      lines ++
      List("}", "")
    ).mkString("\n")
  }     
}
