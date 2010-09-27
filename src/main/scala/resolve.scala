package resolver

import scala.collection.immutable.HashMap

object Resolve {
  private def createMap(nodes: Seq[ast.Node]): HashMap[String, String] = {
    val items = nodes map ((n) => n.id -> n.name)
    HashMap(items:_*)
  }

  def resolveTarget(map: HashMap[String, String])(target: ast.Target): ast.Target =
    map.get(target.getId) match {
      case Some(name) => new ast.ResolvedTarget(target.getId, name)
      case None => target
    }

  def mapAssociation(map: HashMap[String, String])(association: ast.Association): ast.Association =
    new ast.Association(association.name, resolveTarget(map)(association.target), association.cardinality)

  def apply(nodes: Seq[ast.Node]): Seq[ast.Node] = {
    val map = createMap(nodes)
    nodes map ((n) =>
      new ast.Node(n.typ, n.name, n.id,
        n.children map mapAssociation(map)_,
        n.references map mapAssociation(map)_,
        n.fathers map resolveTarget(map)_,
        n.properties
      ) 
    ) 
  }
}
