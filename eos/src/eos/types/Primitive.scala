package eos.types

import ichor.core._

case object Any extends Type {
  override def tName: String = "Any"
  override def tParents: Seq[Type] = Nil
  override def tArgs: Seq[Type] = Nil
}

case object Num extends Type {
  override def tName: String = "Num"
  override def tParents: Seq[Type] = Seq(Any)
  override def tArgs: Seq[Type] = Nil
}

class Fix(val sign: Boolean, val ibits: Int, val fbits: Int) extends Type {
  override def tName: String = (sign,ibits,fbits) match {
    case (true,i,0)  => s"I$i"
    case (false,i,0) => s"U$i"
    case (true,i,f)  => s"I${i}_$f"
    case (false,i,f) => s"U${i}_$f"
  }
  override def tParents: Seq[Type] = Seq(Num)
  override def tArgs: Seq[Type] = Nil
}
object Fix {
  def unapply(x: Type): Option[(Boolean,Int,Int)] = x match {
    case fix: Fix => Some((fix.sign, fix.ibits, fix.fbits))
    case _ => None
  }
}

case object I64 extends Fix(true, 64, 0)
case object U64 extends Fix(false, 64, 0)
case object I32 extends Fix(true, 32, 0)
case object U32 extends Fix(false, 32, 0)

class Flt(val mbits: Int, val ibits: Int) extends Type {
  override def tName = "Flt"
  override def tParents: Seq[Type] = Seq(Num)
  override def tArgs: Seq[Type] = Nil
}

case object FP32 extends Flt(23, 8)
case object FP16 extends Flt(10, 5)
case object FP64 extends Flt(52, 11)
case object BF16 extends Flt(7, 8)