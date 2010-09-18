package backend

trait Printer {
  def print(nodes: Seq[ast.Node]): String
}
