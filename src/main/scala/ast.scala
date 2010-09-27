package ast;

abstract class Target(protected val id:String) {
  def isResolved(): Boolean
  protected val name: String
  def getName:String = name
  def getId:String = id.split(":")(0)
}

class UnresolvedTarget(id: String) extends Target(id) {
  val name: String = "<<unknown name>>"
  def isResolved(): Boolean = false
}

class ResolvedTarget(id: String, val name: String) extends Target(id) {
  def isResolved(): Boolean = true
}

case class Property(val name:String, val typ: String)
case class Association(val name:String, val target:Target, val cardinality: String)

object NodeType extends Enumeration {
  val Concept = Value
  val InterfaceConcept = Value
}

case class Node(
  val typ: NodeType.Value,
  val name: String,
  val id: String,
  val children: Seq[Association],
  val references: Seq[Association],
  val fathers: Seq[Target],
  val properties: Seq[Property]
)
