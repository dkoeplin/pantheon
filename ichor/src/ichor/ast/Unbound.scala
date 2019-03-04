package ichor.ast

import ichor.core._

case class Unbound(name: String) extends Type with AST {
  override def tParents: Seq[Type] = Nil
  override def tArgs: Seq[Type] = Nil
  override def tName: String = name

  override def mirror(f:Tx): Unbound = this
}
