# mps2graph
Graphical visualization of [MPS](http://www.jetbrains.com/mps/) languages

## Dependencies
 * [Scala](http://www.scala-lang.org/)
 * [sbt](http://code.google.com/p/simple-build-tool/)
 * [Graphviz](http://www.graphviz.org/)
 
### Recommendation
 * [ZGRViewer](http://zvtm.sourceforge.net/zgrviewer.html)
 
## Howto
 * execute sbt in the project root
 * type `run -help` to see the available options
 * type `run -i structure.mps -o output.dot -b backend` to generate the graph
 
### Available backends
 * graphviz_complete: the complete grammar
 * graphviz_inheritance: graph only with inheritance relations
 * graphviz_usage: graph only with usage relations
 * graphviz_clustered: inheritance and usage cluster with interconnections
 * text: a textual listing of the language
 
## Status
 * Works for me. Has not been thoroughly tested, yet. So be suspcicious of the generated graphs.
 * Feel free to use it and to report bugs.

## Licence
 * [GPL 2.0](http://www.gnu.org/licenses/gpl-2.0.html)
 
