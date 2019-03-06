package eos.ast

import ichor.core._
import ichor.ast._
import eos.types._

object implicits {

  implicit def Int2Const(x: Int): Const = new Const(x, I32)
  implicit def Long2Const(x: Long): Const = new Const(x, I64)
  implicit def Float2Const(x: Float): Const = new Const(x, FP32)
  implicit def String2Term(x: String): Term = Term(x)
  implicit def String2Sym(x: String): Sym = new Ref(Term(x)).named(x)

  implicit class IntOps(x: Int) {
    def u: Const = new Const(x, U32)
    def U: Const = new Const(x, U64)
  }

  def Block(syms: Ref*): Block = new Block(Nil,syms,syms.last,Effects.Pure)
  implicit def BlockToSome(block: Block): Some[Block] = Some(block)

  implicit class SymASTEquivalence(a: Sym) {
    def =:=(b: Sym): Boolean = ASTEquivalent(a, b)
  }
  implicit class DefASTEquivalence(a: Def) {
    def =:=(b: Def): Boolean = ASTEquivalent(a, b)
  }
  implicit class BlockASTEquivalence(a: Block) {
    def =:=(b: Block): Boolean = ASTEquivalent(a, b)
  }

  def ASTEquivalent(a: Any, b: Any): Boolean = (a,b) match {
    case (a: Param, b: Param) =>
      a.c == b.c && a.tp =:= b.tp
    case (a: Const, b: Const) =>
      a.c == b.c && a.tp =:= b.tp
    case (a: Bound, b: Bound) => true
    case (a: Ref, b: Ref) =>
      ASTEquivalent(a.rhs, b.rhs)

    case (a: Block, b: Block) =>
      a.stms.zip(b.stms).forall{case (a,b) => ASTEquivalent(a,b) } &&
      ASTEquivalent(a.result, b.result)

    case (a: Iterator[_], b: Iterator[_]) =>
      a.zip(b).forall{case (x,y) => ASTEquivalent(x,y) }

    case (a: Iterable[_], b: Iterable[_]) =>
      a.zip(b).forall{case (x,y) => ASTEquivalent(x,y) }

    case (a: Product, b: Product) =>
      a.getClass == b.getClass &&
      ASTEquivalent(a.productIterator, b.productIterator)

    case _ => a == b
  }

  def ast(x: Any): String = x match {
    case a: Const => s"Const(${a.c},${a.tp})"
    case a: Param => s"Param(${a.c},${a.tp})"
    case a: Bound => s"<Bound>"
    case a: Ref   => s"Ref(${ast(a.rhs.get)})"
    case a: Def   => s"${a.productPrefix}(${a.productIterator.map(ast).mkString(", ")})"
    case b: Block => s"Block(${b.stms.map(ast).mkString(",")}"
    case Nil       => s"Nil"
    case s: Seq[_] => s"Seq(${s.map(ast).mkString(", ")})"
    case Some(a)   => s"Some(${ast(a)})"
    case None      => s"None"
    case _ => x.toString
  }
}
