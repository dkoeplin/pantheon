package ichor

import ichor.core.{Block, Sym}

abstract class Traversal extends Pass {
  def recurse: Boolean = true

  def run(graph: Block): Block = {
    visit(graph)
    graph
  }

  def visit(block: Block): Unit = {
    block.stms.foreach(traverse)
  }
  private def traverse(sym: Sym): Unit = {
    visit(sym)
    if (recurse) sym.op.foreach(_.blocks.foreach(visit))
  }

  def visit(sym: Sym): Unit

}
