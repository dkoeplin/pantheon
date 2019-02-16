package ichor

package object core {
  type Stm = (Ref,Def)

  implicit class MetadataMethods(s: Sym) {
    def getMeta[M<:Meta[M]:Manifest]: Option[M] = s.meta.get(manifest[M].runtimeClass).map(_.asInstanceOf[M])
    def setMeta(m: Meta[_]): Unit = s.meta += m.key -> m
  }

  implicit class EffectMethods(s: Sym) {
    def effects: Effects = s.getMeta[Effects].getOrElse(Effects.Pure)
    def effects_=(e: Effects): Unit = s.setMeta(e)

    def isMutable: Boolean = s.effects.mutable
  }

  def syms(a: Any*): Set[Sym] = a.flatMap{
    case s: Sym         => Seq(s)
    case b: Block       => b.result +: b.effects.antiDeps
    case d: Def         => d.inputs
    case i: Iterator[_] => i.flatMap(e => syms(e))
    case i: Iterable[_] => i.flatMap(e => syms(e))
    case p: Product     => p.productIterator.flatMap(e => syms(e))
    case _              => Nil
  }.toSet

  def dyns(a: Any*): Set[Sym] = a.flatMap{
    case s: Dyn         => Seq(s)
    case b: Block       => (b.result +: b.effects.antiDeps).collect{case d: Dyn => d}
    case d: Def         => dyns(d.inputs)
    case i: Iterator[_] => i.flatMap(e => dyns(e))
    case i: Iterable[_] => i.flatMap(e => dyns(e))
    case p: Product     => p.productIterator.flatMap(e => dyns(e))
    case _              => Nil
  }.toSet

  def collectBlocks(a: Any*): Seq[Block] = a.flatMap{
    case b: Block       => Seq(b)
    case d: Def         => d.blocks
    case i: Iterator[_] => i.flatMap(e => collectBlocks(e))
    case i: Iterable[_] => i.flatMap(e => collectBlocks(e))
    case p: Product     => p.productIterator.flatMap(e => collectBlocks(e))
    case _ => Nil
  }

}
