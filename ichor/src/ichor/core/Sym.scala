package ichor.core

import scala.collection.mutable

/** Any symbol. */
sealed abstract class Sym extends Serializable {
  def rhs: Option[Def] = None
  def stm: Option[Stm] = None
  def imm: Option[Any] = None
  lazy val meta = mutable.HashMap.empty[Class[_],Meta[_]]

  override def toString: String = s"x$hashCode"
}

/** Any symbol which has a value which isn't defined until runtime. */
sealed abstract class Dyn extends Sym

/** A reference to the output of an operation. */
final class Ref(df: Def) extends Dyn {
  override def rhs: Option[Def] = Some(df)
  override def stm: Option[Stm] = Some((this,df))
}

/** A placeholder value. */
final class Bound extends Dyn




/** Any symbol which has a known value at compile time. */
sealed abstract class Val extends Sym

case object Error extends Val

final class Const(val c: Any, val tp: Type) extends Val {
  override def imm: Option[Any] = Some(c)
  override def toString: String = tp.constString(c)

  override def equals(obj: Any): Boolean = obj match {
    case that: Const => this.c == that.c && this.tp =:= that.tp
    case _ => false
  }
  override def hashCode(): Int = (c,tp).hashCode()
}

final class Param(var c: Any, val tp: Type) extends Val {
  override def imm: Option[Any] = Some(c)
}

