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
)
object Prefix {
  val DEF = Prefix(DEF=true)
  val VAR = Prefix(VAR=true)
  val VAL = Prefix(VAL=true)
  val None = Prefix()
}