package frontend 

import scala.xml

import scala.xml.PrettyPrinter;


object Parser {
  def parse(document: xml.Node): List[ast.Node] = {
    val nodes = document \ "node"
    Console.print(new PrettyPrinter(80, 5).formatNodes(nodes))
    Nil
  }

  def resolve(nodes: List[ast.Node]): List[ast.Node] = Nil
}
