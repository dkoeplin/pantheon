package ichor.core

abstract class Type extends Meta[Type](Transfer.Mirror) with Product {
  def tParents: Seq[Type]
  def tArgs: Seq[Type]
  def tName: String = this.productPrefix

  /** True if this Type and that Type are equivalent. */
  def =:=(that: Type): Boolean = {
    this.getClass == that.getClass && this.tArgs.zip(that.tArgs).forall{case (a,b) => a =:= b }
  }

  /** True if this Type is a subclass of that Type. */
  def <:<(that: Type): Boolean = this =:= that || this.tParents.exists{_ <:< that}

  /** True if this Type is a superclass of that Type. */
  def >:>(that: Type): Boolean = that <:< this



  override def toString: String = tName + (if (tArgs.nonEmpty) s"[${tArgs.mkString(",")}]" else "")
}
