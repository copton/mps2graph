import frontend.Parser
import scala.xml.XML

object Control {
  def main(args:Array[String]) = {
    val options = Options(args)

    val document = XML.loadFile(options.infile)
    val nodes = Parser.parse(document)

    val result = options.backend match {
      case "graphviz" => backend.graphviz.Printer.print(nodes)
      case "text" => backend.text.Printer.print(nodes)
      case _ => Options.usage()
    }
    
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
