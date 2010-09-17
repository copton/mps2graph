package frontend 

import scala.xml

import scala.xml.PrettyPrinter;


object Parser {
  def parse(document: xml.Node): Seq[ast.Node] = 
    (document \ "node") map parseNode

  private def parseNode(node: xml.Node):ast.Node = 
    node\"@type" text match {
      case "jetbrains.mps.lang.structure.structure.ConceptDeclaration:0" => {
        val name = (node\"property").filter(_ \ "@name" == "name:0") //\ "@value"
        System.out.println(node)
        null
      }
      case _ => null
    }

  def resolve(nodes: List[ast.Node]): List[ast.Node] = Nil
}
