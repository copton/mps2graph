package backend
package graphviz_clustered

object Printer extends backend.Printer {
  object Inheritance {
    private def printInheritance(nodes: Seq[ast.Node]): Seq[String] =
      for {
        node <- nodes
        father <- node.fathers
      } yield "\"I_%s\" -> \"I_%s\" [arrowhead=empty,samehead=\"I_%s_father\"];".format(node.name, father, father)

    private def printProperties(node: ast.Node): Seq[String] =
      node.properties map ((p) => "%s: %s".format(p.name, p.typ))

    private def printReferences(node: ast.Node): Seq[String] =
      node.references.map ((r) => "%s: *%s (%s)".format(r.name, r.target, r.cardinality))

    private def printChildren(node: ast.Node): Seq[String] =
      node.children.map ((r) => "%s: %s (%s)".format(r.name, r.target, r.cardinality))

    private def printNode(node: ast.Node): String = {
      val members = (
          printProperties(node) ++
          printChildren(node) ++
          printReferences(node))

      if (members.size == 0) {
        "\"I_%s\" [label=\"%s\",shape=%s];".format(
          node.name, node.name, node.typ match {
            case ast.NodeType.Concept => "rectangle"
            case ast.NodeType.InterfaceConcept => "ellipse"
          }
        )
      } else {
        "\"I_%s\" [shape=%s, label=\"{%s|%s}\"];".format(
          node.name, node.typ match {
            case ast.NodeType.Concept => "record"
            case ast.NodeType.InterfaceConcept => "Mrecord"
          }, node.name, members mkString "\\l"
        ) 
      }
    }

    private def printNodes(nodes: Seq[ast.Node]): Seq[String] = 
      nodes map printNode
    
    def print(nodes: Seq[ast.Node]): Seq[String] =
      List(
        "subgraph cluster_inheritance {"
      ) ++
      printNodes(nodes) ++
      printInheritance(nodes) ++
      List("}")

  }

  object Usage {
    private def printChildren(nodes: Seq[ast.Node]): Seq[String] =
      for {
        node <- nodes
        child <- node.children
      } yield "\"U_%s\":\"%s\" -> \"U_%s\" [arrowhead=diamond, samehead=\"U_%s_child\"];".format(
          node.name, child.name, child.target, child.target
        )

    private def printReferences(nodes: Seq[ast.Node]): Seq[String] =
      for {
        node <- nodes
        reference <- node.references
      } yield "\"U_%s\":\"%s\" -> \"U_%s\" [arrowhead=ediamond, samehead=\"U_%s_reference\"];".format(
          node.name, reference.name, reference.target, reference.target
        )

    def printChildMembers(children: Seq[ast.Association]): Seq[String] =
      children map ((c) =>
        "<%s>%s: %s (%s)".format(c.name, c.name, c.target, c.cardinality))

    def printReferenceMembers(references: Seq[ast.Association]): Seq[String] =
      references map ((r) =>
        "<%s>%s: %s (%s)".format(r.name, r.name, r.target, r.cardinality))

    def printNode(node: ast.Node): String =
      "\"U_%s\" [shape=%s,label=\"{%s|%s}\"];".format(
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
      )

    def printNodes(nodes: Seq[ast.Node]): Seq[String] =
      nodes map printNode

    def print(nodes: Seq[ast.Node]): Seq[String] =
      List(
        "subgraph cluster_usage {"
      ) ++
      printNodes(nodes) ++
      printChildren(nodes) ++
      printReferences(nodes) ++
      List("}")
  }

  object Link {
    def printIU(node: ast.Node): String =
      "\"I_%s\" -> \"U_%s\" [arrowhead=none, style=dotted];".format(node.name, node.name)

    def printUI(node: ast.Node): String =
      "\"U_%s\" -> \"I_%s\" [arrowhead=none, style=dotted];".format(node.name, node.name)

    def print(nodes: Seq[ast.Node]): Seq[String] =
      (nodes map printIU) ++ (nodes map printUI)

  }

  def print(nodes: Seq[ast.Node]): String =
    (
      List(
        "digraph structure {"
      ) ++ 
      Inheritance.print(nodes) ++
      Usage.print(nodes) ++
      Link.print(nodes) ++
      List(
        "}", 
        ""
      )
    ).mkString("\n")
}
