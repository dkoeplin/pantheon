package ichor

import ichor.core.{Block, Sym}

trait Transformer {
  protected def f[A](x: A): A = apply(x)

  def apply[A](x: A): A = (x match {
    case s: Sym         => transformSym(s)
    case b: Block       => transformBlock(b)
    case i: Iterable[_] => i.map(f(_))
    case _ => x
  }).asInstanceOf[A]

  protected def transformSym(s: Sym): Sym
  protected def transformBlock(b: Block): Block
}
