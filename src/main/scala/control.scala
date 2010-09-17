import frontend.Parser
import backend.graphviz.Printer
import scala.xml.XML

object Control {
  def main(args:Array[String]) = {
    if( args.length == 1 ) { // must have one arg
      val document = XML.loadFile(args(0))
      val nodes = Parser.parse(document)
      val result = Printer.print(nodes)
      Console.print(result)
    }
    else error("need one arg");
  }
}
