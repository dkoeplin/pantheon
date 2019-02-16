package ichor

import ichor.core.Graph

import java.nio.file.{Files,Paths}

object Main {

  def main(args: Array[String]): Unit = {
    val file = "ichor/test/test.y"
    val parser = new ichor.ast.Parser
    val body = parser(file).getOrElse(sys.exit(-1))

    val graph = new Graph
    val codePrinter = ichor.ast.CodePrinter(graph)
    val astPrinter  = ichor.ast.ASTPrinter(graph)

    Files.createDirectories(Paths.get(graph.logDir))

    graph.dbg.in(graph.logDir, "code"){ codePrinter.run(body) }
    graph.dbg.in(graph.logDir, "ast"){ astPrinter.run(body) }
  }

}
