package ichor

import scala.collection.mutable

package object core {
  type Stm = (Ref,Def)
  type Tx = ichor.Transformer

  implicit class MetadataMethods(s: Sym) {
    def getMeta[M<:Meta[M]:Manifest]: Option[M] = s.meta.get(manifest[M].runtimeClass).map(_.asInstanceOf[M])
    def setMeta(m: Meta[_]): Unit = s.meta += m.key -> m
  }

  implicit class EffectMethods(s: Sym) {
    def effects: Effects = s.getMeta[Effects].getOrElse(Effects.Pure)
    def effects_=(e: Effects): Unit = s.setMeta(e)

    def isMutable: Boolean = s.effects.mutable
  }

  implicit class NameMethods[S<:Sym](s: S) {
    def getName: Option[String] = s.getMeta[Name].map(_.name)
    def name_=(name: String): Unit = s.setMeta(Name(name))
    def named(name: String): S = { s.setMeta(Name(name)); s }
  }

  implicit class SrcCtxMethods[S<:Sym](s: S) {
    def ctx: SrcCtx = s.getMeta[SrcCtx].getOrElse(SrcCtx.empty)
    def ctx_=(ctx: SrcCtx): Unit = s.setMeta(ctx)
    def at(ctx: SrcCtx): S = { s.setMeta(ctx); s }
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
  }.toSeq


  /** Computes an *external* summary for a sequence of nodes with effects in a single scope.
    * Ignores reads/writes on data allocated within the scope.
    * TODO: May want to include reads/writes on values allocated inside if they escape.
    */
  def summzarizeScopeEffects(impure: Iterable[Sym]): Effects = {
    val allocs = mutable.HashSet.empty[Sym]
    var simple = false
    var global = false
    var throws = false
    val reads  = mutable.HashSet.empty[Sym]
    val writes = mutable.HashSet.empty[Sym]
    impure.foreach{s =>
      if (s.isMutable) allocs += s
      simple ||= simple
      global ||= global
      throws ||= throws
      reads ++= s.effects.reads
      writes ++= s.effects.writes
    }
    Effects(
      simple = simple,
      global = global,
      throws = throws,
      reads  = reads.toSet diff allocs,
      writes = writes.toSet diff allocs,
    )
  }

}
