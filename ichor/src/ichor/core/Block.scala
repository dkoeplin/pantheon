package ichor.core

case class Block(stms: Seq[Ref], result: Ref, effects: Effects)
