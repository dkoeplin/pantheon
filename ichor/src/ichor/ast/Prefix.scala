package ichor.ast

/** Prefixes to declarations.
  *
  * @param DEF call by name. RHS is run every full reference.
  * @param VAL call by reference. If params is Nil, RHS is evaluated on declaration.
  * @param VAR mutable declaration. RHS can be reassigned.
  */
case class Prefix(
  DEF: Boolean = false,
  VAL: Boolean = false,
  VAR: Boolean = false
) {
  override def toString: String = {
    ((if (DEF) Some("Prefix.DEF") else None) ++
    (if (VAL) Some("Prefix.VAL") else None) ++
    (if (VAR) Some("Prefix.VAR") else None)) mkString " | "
  }
  def |(that: Prefix): Prefix = Prefix(
    this.DEF | that.DEF,
    this.VAL | that.VAL,
    this.VAR | that.VAR
  )
}
object Prefix {
  val DEF = Prefix(DEF=true)
  val VAR = Prefix(VAR=true)
  val VAL = Prefix(VAL=true)
  val None = Prefix()
}