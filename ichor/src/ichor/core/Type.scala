package ichor.core

import utils.escapeConst

abstract class Type extends Meta[Type](Transfer.Mirror) with Equals with Serializable {
  def tParents: Seq[Type]
  def tArgs: Seq[Type]
  def tName: String

  /** True if this Type and that Type are equivalent. */
  def =:=(that: Type): Boolean = {
    this.getClass == that.getClass && this.tArgs.zip(that.tArgs).forall{case (a,b) => a =:= b }
  }

  /** True if this Type is a subclass of that Type. */
  def <:<(that: Type): Boolean = this =:= that || this.tParents.exists{_ <:< that}

  /** True if this Type is a superclass of that Type. */
  def >:>(that: Type): Boolean = that <:< this

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Type]

  override def equals(obj: Any): Boolean = obj match {
    case that: Type => this =:= that
    case _ => false
  }

  def constString(c: Any): String = s"$tName(${escapeConst(c)})"

  override def toString: String = tName
}
