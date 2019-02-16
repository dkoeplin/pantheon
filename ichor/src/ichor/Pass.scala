package ichor

import ichor.core.Block

trait Pass {
  final def name: String = this.getClass.getSimpleName

  def run(graph: Block): Block
}
