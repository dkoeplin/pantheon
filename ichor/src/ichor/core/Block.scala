package ichor.core

class Block(val inputs: Seq[Dyn], val stms: Seq[Ref], val result: Sym, val effects: Effects)
