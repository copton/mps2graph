import frontend.Parser
import scala.xml.XML

object Control {
  def main(args:Array[String]) = {
    val options = Options(args)

    val document = XML.loadFile(options.infile)
    val nodes = Parser.parse(document)

    val printer = options.backend match {
      case "graphviz" => import backend.graphviz.Printer; Printer
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
