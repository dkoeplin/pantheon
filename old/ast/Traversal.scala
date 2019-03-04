package ichor.ast

abstract class Traversal {

  def run(graph: Scope): Scope = {
    visit(graph)
    graph
  }

  def visit(node: AST): Unit = { }

}
