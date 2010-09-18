package frontend 

import scala.xml

import scala.xml.PrettyPrinter;

object Parser {
  private def has_attr(node: xml.Node, key: String, value: String): Boolean =
    node.attribute(key).isDefined && node.attribute(key).get(0).text == value

  private def has_attr(node: xml.Node, key:String): Boolean =
    node.attribute(key).isDefined

  private def get_attr(node: xml.Node, key:String): String =
    node.attribute(key).get(0).text

  private def children(node: xml.Node, label: String): xml.NodeSeq =
    for {
      child <- node.child
      if child.label == label
    } yield child

  def parse(document: xml.Node): Seq[ast.Node] = 
    children(document, "node") map parseNode

  private def getImplements(node: xml.Node): Seq[String] =
    for {
      sub_node <- children(node, "node")
      if has_attr(sub_node, "role", "implements:0")
      link <- children(sub_node, "link")
      if has_attr(link, "resolveInfo")
    } yield get_attr(link, "resolveInfo")

  private def getExtends(node: xml.Node): Seq[String] = { 
    val type1 = for {
      sub_node <- children(node, "node")
      if has_attr(sub_node, "role", "extends:0")
      link <- children(sub_node, "link")
      if has_attr(link, "resolveInfo")
    } yield get_attr(link, "resolveInfo")
      
    val type2 = for {
      link <- children(node, "link")
      if has_attr(link, "role", "extends:0")
      if has_attr(link, "resolveInfo")
    } yield get_attr(link, "resolveInfo")

    type1 ++ type2
  }

  private def getProperties(node: xml.Node): Seq[ast.Property] =
    for {
      sub_node <- children(node, "node")
      if has_attr(sub_node, "role", "propertyDeclaration:0")
      property <- children(sub_node, "property")
      if has_attr(property, "value")
      if has_attr(property, "resolveInfo")
    } yield ast.Property(get_attr(property, "value"), get_attr(property, "resolveInfo"))

  private def getName(node: xml.Node): String = {
    val values = for {
      property <- children(node, "property")
      if has_attr(property, "name", "name:0")
      if has_attr(property, "value")
    } yield get_attr(property, "value")
    values(0)
  }

  private def debug(foo: AnyRef): Boolean = {
    Console.print(foo)
    Console.print("\n")
    true
  }

  private def getChildren(node: xml.Node): Seq[ast.Association] =
    getAssociation("aggregation", node)

  private def getReferences(node: xml.Node): Seq[ast.Association] =
    getAssociation("reference", node)

  private def getAssociation(association: String, node: xml.Node): Seq[ast.Association] =
    for {
      sub_node <- children(node, "node")
      if has_attr(sub_node, "role", "linkDeclaration:0")
      if children(sub_node, "property") exists ((n) => 
        has_attr(n, "name", "metaClass:0") &&
        has_attr(n, "value", association))

      name <- children(sub_node, "property")
      if has_attr(name, "name", "role:0")
      if has_attr(name, "value")

      cardin <- children(sub_node, "property")
      if has_attr(cardin, "name", "sourceCardinality:0")
      if has_attr(cardin, "value")

      target <- children(sub_node, "link")
      if has_attr(target, "role", "target:0")
      if has_attr(target, "resolveInfo")
      
    } yield ast.Association(get_attr(name, "value"), get_attr(target, "resolveInfo"), get_attr(cardin, "value"))

  private def getType(node: xml.Node): ast.NodeType.Value = 
    node.attribute("type").get(0).text match {
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
