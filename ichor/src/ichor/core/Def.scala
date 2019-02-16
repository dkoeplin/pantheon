package ichor.core

abstract class Def extends Product with Serializable {
  final type Tx = ichor.Transformer

  def inputs: Seq[Sym] = syms(productIterator).toSeq

  def mutableInputs: Seq[Sym] = inputs.filter(_.isMutable)

  def rewrite: Sym = null

  def update(t: Tx): Unit = { }

  def mirror(f: Tx): Def = {
    val args = this.productIterator.toSeq.map(f(_)).asInstanceOf[Seq[Object]]
    this.getClass.getConstructors.head.newInstance(args:_*).asInstanceOf[Def]
  }

  def blocks: Iterator[Block] = this.productIterator.collect{case b: Block => b}

  def effects: Effects = blocks.map(_.effects).foldLeft(Effects.Pure){_ andAlso _} andAlso Effects.Reads(mutableInputs:_*)
}
