package ichor.core

/** The effects summary of a symbol.
  *
  * Effects metadata is "Ignore" since it should never be removed, but we always take the "old"
  * metadata during mirroring since this is always created at staging time.
  *
  * @param unique Should not be CSEd
  * @param sticky Should not be code motioned out of blocks
  * @param simple Requires ordering with respect to other simple effects
  * @param global Modifies execution of the entire program (e.g. exceptions, exiting)
  * @param mutable Allocates a mutable structure
  * @param throws May throw exceptions (so speculative execution is unsafe)
  * @param reads A set of read symbols
  * @param writes A set of written symbols
  * @param antiDeps Anti-dependencies of this operation (must come before this symbol)
  */
case class Effects(
    unique:  Boolean = false,
    sticky:  Boolean = false,
    simple:  Boolean = false,
    global:  Boolean = false,
    mutable: Boolean = false,
    throws:  Boolean = false,
    reads:   Set[Sym] = Set.empty,
    writes:  Set[Sym] = Set.empty,
    antiDeps: Seq[Sym] = Nil)
  extends Meta[Effects](transfer = Transfer.Ignore) {

  private def combine(that: Effects, m1: Boolean, m2: Boolean) = Effects(
    unique  = this.unique || that.unique,
    sticky  = this.sticky || that.sticky,
    simple  = this.simple || that.simple,
    global  = this.global || that.global,
    mutable = (m1 && this.mutable) || (m2 && that.mutable),
    throws  = this.throws || that.throws,
    reads   = this.reads union that.reads,
    writes  = this.writes union that.writes
  )
  def orElse(that: Effects): Effects = this.combine(that, m1 = false, m2 = false)
  def andAlso(that: Effects): Effects = this.combine(that, m1 = true, m2 = true)
  def andThen(that: Effects): Effects = this.combine(that, m1 = false, m2 = true)
  def star: Effects = this.copy(mutable = false) // Pure orElse this

  def isPure: Boolean = this == Effects.Pure
  def isMutable: Boolean = mutable
  def isIdempotent: Boolean = !simple && !global && !mutable && writes.isEmpty && !throws
  def mayCSE: Boolean = isIdempotent && !unique

  def mayWrite(ss: Set[Sym]): Boolean = global || ss.exists{s => writes contains s }
  def mayRead(ss: Set[Sym]): Boolean = global || ss.exists{s => reads contains s }




  /** Find scheduling dependencies in context
    * WAR - always include reads as scheduling dependencies of writes
    * "AAA" - always include allocation as scheduling dependencies of an access (read or write)
    * RAW/WAW - include the *most recent* write as scheduling dependency of an access ("AAW" - access after write)
    * simple - include the *most recent* previous simple effect as a scheduling dependency of a simple effect
    * global - include ALL global effects as scheduling dependencies of a global effect
    */
  def inContext(impure: Iterable[Sym]): Effects = {
    val antideps = if (global) impure
    else {
      val accesses = reads ++ writes  // Cannot read/write prior to allocation

      /** True if the symbol is an allocation of an accessed memory.
        * TODO: Account for aliasing?
        */
      def isAllocHazard(s: Sym): Boolean = reads.contains(s) || writes.contains(s)

      /** True if the symbol s reads a memory for which this effects includes a write.
        * (If true, the symbol s must be an anti-dependency of these effects)
        * TODO: Account for aliasing?
        */
      def isWARHazard(s: Sym): Boolean = s.effects.mayRead(writes)

      // RAW / WAW
      var unwrittenAccesses: Set[Sym] = accesses // Reads/writes for which we have not yet found a previous writer

      /** True if the symbol s is the first write found to a memory accessed by these effects.
        * (If true, the symbol s must be an anti-dependency of these effects)
        * TODO: Account for aliasing?
        */
      def isAAWHazard(s: Sym): Boolean = {
        if (unwrittenAccesses.nonEmpty) {
          val (written, unwritten) = unwrittenAccesses.partition(s.effects.writes.contains)
          unwrittenAccesses = unwritten
          written.nonEmpty
        }
        else false
      }

      // 1) Reads and writes are ordered w.r.t. writes and allocations. Writes are ordered w.r.t. reads.
      val hazards = impure.filter{s => isWARHazard(s) || isAAWHazard(s) || isAllocHazard(s) }
      // 2) Simple effect is ordered w.r.t. the last simple effect in scope
      val simpleDep = if (simple) impure.find{s => s.effects.simple } else None
      // 3) All effects are ordered w.r.t. the last global effect in scope
      val globalDep = impure.find{s => s.effects.global }

      hazards ++ simpleDep ++ globalDep
    }
    this.copy(antiDeps = antideps)
  }

  override def toString: String = {
    if      (this == Effects.Pure)    "Pure"
    else if (this == Effects.Unique)  "Unique"
    else if (this == Effects.Sticky)  "Sticky"
    else if (this == Effects.Mutable) "Mutable"
    else if (this == Effects.Simple)  "Simple"
    else if (this == Effects.Global)  "Global"
    else if (this == Effects.Throws)  "Throws"
    else {
      "(" +
        ((if (this.unique) List(s"unique=${this.unique}") else Nil) ++
          (if (this.sticky) List(s"sticky=${this.sticky}") else Nil) ++
          (if (this.simple) List(s"simple=${this.simple}") else Nil) ++
          (if (this.global) List(s"global=${this.global}") else Nil) ++
          (if (this.mutable) List("mutable") else Nil) ++
          (if (this.throws) List("throws") else Nil) ++
          (if (this.reads.nonEmpty) List(s"""reads={${this.reads.map(x=> s"$x").mkString(",")}}""") else Nil) ++
          (if (this.writes.nonEmpty) List(s"""writes={${this.writes.map(x=> s"$x").mkString(",")}}""") else Nil)).mkString(", ") + ")"
    }
  }
}

object Effects {
  lazy val Pure = Effects()
  lazy val Sticky = Effects(sticky = true)
  lazy val Unique = Effects(unique = true)
  lazy val Simple = Effects(simple = true)
  lazy val Global = Effects(global = true)
  lazy val Mutable = Effects(mutable = true)
  lazy val Throws = Effects(throws = true)

  def Writes(x: Sym*) = Effects(writes = x.toSet)
  def Reads(x: Sym*) = Effects(reads = x.toSet)
}
