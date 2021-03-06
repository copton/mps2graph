import frontend.Parser
import scala.xml.XML

object Control {
  def main(args:Array[String]) = {
    val options = Options(args)

    val document = XML.loadFile(options.infile)
    val nodes = filter.UnresolvedTargets(resolver.Resolve(Parser.parse(document)))

    val printer = options.backend match {
      case "graphviz_complete" => import backend.graphviz_complete.Printer; Printer
      case "graphviz_inheritance" => import backend.graphviz_inheritance.Printer; Printer
      case "graphviz_usage" => import backend.graphviz_usage.Printer; Printer
      case "text" => import backend.text.Printer; Printer
      case _ => Options.usage()
    }

    val result = printer.print(nodes)
    
    options.outfile match {
      case "-" => Console.print(result)
      case _ => try {
                  val out = new java.io.BufferedWriter(new java.io.FileWriter(options.outfile))
                  out.write(result)
                  out.close
                } catch {
                  case e: Exception => Console.print(e.getMessage())
                }
    }
  }
}
