package ichor.core

case object Unk extends Type {
  override def tName: String = "Unk"
  override def tArgs: Seq[Type] = Nil
  override def tParents: Seq[Type] = Nil
}
