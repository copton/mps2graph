package ast;

trait Node
{
  val name: String
  val children: List[Child] = Nil
  val references: List[Reference] = Nil
}

trait Target {
  val name: String
  val reference: Option[Node] = None
}

case class Reference(val name: String) extends Target
case class Child(val name: String) extends Target

case class InterfaceConcept(val name: String) extends Node
case class Concept(val name: String) extends Node
