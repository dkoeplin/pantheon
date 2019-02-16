package ichor

import ichor.core._

abstract class Traversal extends Pass {
  def recurse: Boolean = true

  def run(graph: Block): Block = {
    visit(graph)
    graph
  }

  def visit(block: Block): Unit = {
    block.stms.foreach(traverse)
  }
  private def traverse(ref: Ref): Unit = {
    visit(ref)
    if (recurse) ref.rhs.foreach(_.blocks.foreach(visit))
  }

  def visit(ref: Ref): Unit

}
