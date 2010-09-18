package backend
package text

object Printer extends backend.Printer {
  private def print(property: ast.Property): String =
    property.name + ": " + property.typ

  private def print(association: ast.Association): String =
    association.name + ": " + association.target + " (" + association.cardinality + ")"

  private def print(father: String): String = 
    father

  private def print(typ: ast.NodeType.Value): String =
    typ match {
      case ast.NodeType.Concept => "Concept"
      case ast.NodeType.InterfaceConcept => "InterfaceConcept"
    }

  private def printNode(node: ast.Node): String = List(
      node.name,
      "\ttype: " + print(node.typ),
      "\tfathers: " + (node.fathers map print).mkString(", "),
      "\tchildren: " + (node.children map print).mkString(", "),
      "\treferences: " + (node.references map print).mkString(", "),
      "\tproperties: " + (node.properties map print).mkString(", ")
    ).mkString("\n")
    

  def print(nodes: Seq[ast.Node]): String =
    (nodes map printNode).mkString("\n")
}
