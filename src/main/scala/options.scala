class Options(
  val infile: String,
  val backend: String,
  val outfile: String
)

object Options {
  def apply(args: Array[String]): Options = {
    var infile: Option[String] = None
    var backend: Option[String] = None
    var outfile: Option[String] = None

    var i = 0
    while(i+1 < args.size) {
      args(i) match {
        case "-i" => infile = Some(args(i+1))
        case "-b" => backend = Some(args(i+1))
        case "-o" => outfile = Some(args(i+1))
        case _ => usage()
      }
      i += 2
    }
    if (i == args.size -1 || ! infile.isDefined || ! backend.isDefined || ! outfile.isDefined) {
      usage()
    } else {
      new Options(infile.get, backend.get, outfile.get)
    }
  }

  def usage(): Nothing = {
    Console.print("""valid parameters:
-i <filename>: the input XML file according to MPS schema for language structures
-b <backend>: the backend. Possible values are: text, graphviz_complete, graphviz_inheritance, graphviz_clustered
-o <filename|-> the output file. - means standard output.
""")
    System.exit(1)
    throw null // System.exit returns Unit but we need Nothing.
  }
}

