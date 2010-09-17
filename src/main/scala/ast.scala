package ast;

case class Property(val name:String, val typ: String)
case class Association(val target:String, val cardinality: String)

object NodeType extends Enumeration {
  val Concept = Value
  val InterfaceConcept = Value
}

case class Node(
  val typ: NodeType.Value,
  val name: String,
  val children: Seq[Association],
  val references: Seq[Association],
  val fathers: Seq[String],
  val properties: Seq[Property]
)
