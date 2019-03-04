package ichor.core

import scala.collection.Seq

class Block(
    val inputs:  Seq[Dyn],
    val stms:    Seq[Ref],
    val result:  Sym,
    val effects: Effects
)
