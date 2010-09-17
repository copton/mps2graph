package frontend 

import scala.xml

import scala.xml.PrettyPrinter;

class RichNodeSeq(val nodes:xml.NodeSeq) {
  def ~@(attr: (String, String)): xml.NodeSeq =
    nodes.filter((n) => {
      val a = n.attribute(attr._1)
      a.isDefined && a.get.text == attr._2
    })

  def ~@(attr: String): xml.NodeSeq = nodes.filter(_.attribute(attr).isDefined)

  def ~<>(label: String): xml.NodeSeq = nodes.flatMap(_.child.filter(_.label == label))

  def ->@(attr: String): Seq[String] = 
    nodes.filter(_.attribute(attr).isDefined).map(_.attribute(attr).get.text)
}

object Parser {
  implicit def enrichNodeSeq(nodes: xml.NodeSeq):RichNodeSeq = new RichNodeSeq(nodes)

  def parse(document: xml.Node): Seq[ast.Node] = 
    (document \ "node") map parseNode

  private def getImplements(node: xml.Node): Seq[String] =
        node ~<> "node" ~@ ("role", "implements:0") ~<> "link" ->@ "resolveInfo"

  private def getExtends(node: xml.Node): Seq[String]= 
        (node ~<> "node" ~@ ("role", "extends:0") ~<> "link" ->@ "resolveInfo") ++
        (node ~<> "link" ~@ ("role", "extends:0") ->@ "resolveInfo")

  private def getProperties(node: xml.Node): Seq[ast.Property] = {
    val nodes = node ~<> "node" ~@ ("role", "propertyDeclaration:0")
    
    for {
      node <- nodes
      name = node ~<> "property" ->@ "value"
      type_ = node ~<> "link" ->@ "resolveInfo"
    } yield ast.Property(name(0), type_(0))
  }

  private def getName(node: xml.Node): String =
    (node ~<> "property" ~@ ("name", "name:0") ->@ "value")(0)

  private def getChildren(node: xml.Node): Seq[ast.Association] = {
    Nil 
  }

  private def getReferences(node: xml.Node): Seq[ast.Association] = {
    Nil 
  }

  private def getType(node: xml.Node): ast.NodeType.Value = (node ->@ "type")(0) match {
    case "jetbrains.mps.lang.structure.structure.ConceptDeclaration:0" =>
      ast.NodeType.Concept
    case "jetbrains.mps.lang.structure.structure.InterfaceConceptDeclaration:0" =>
      ast.NodeType.InterfaceConcept
  }

  private def parseNode(node: xml.Node): ast.Node =
    ast.Node(
      typ = getType(node),
      name = getName(node),
      children = getChildren(node),
      references = getReferences(node),
      fathers = getImplements(node) ++ getExtends(node),
      properties = getProperties(node)
    )
}
