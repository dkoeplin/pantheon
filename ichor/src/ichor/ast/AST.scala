package ichor.ast

import ichor.core._

trait AST extends Def {
  var ctx: SrcCtx = SrcCtx.empty
  def at(t: SrcCtx): this.type = { ctx = t; this }
}

/** A reference to a symbol with the given name. */
case class Term(name: String) extends AST {
  override def at(t: SrcCtx): this.type = {
    ctx = t.copy(col = t.col - name.length)
    this
  }
}

case class BlockExp(block: Block) extends AST

/** A function, val, or var declaration. */
abstract class Decl extends AST {
  def prefix: Prefix
  def term: Term
  def params: Seq[Seq[ValDef]]
  def tp: Option[Type]
  def body: Option[Block]
}

/** A function definition.
  *
  * @param prefix
  * @param term
  * @param params
  * @param tp
  * @param body
  */
case class DefDef(
    prefix: Prefix,
    term:   Term,
    params: Seq[Seq[ValDef]],
    tp:     Option[Type],
    body:   Option[Block])
   extends Decl

/** A immutable value definition or mutable value initial declaration.
  *
  * @param prefix
  * @param term
  * @param tp
  * @param body
  */
case class ValDef(
     prefix: Prefix,
     term:   Term,
     tp:     Option[Type],
     body:   Option[Block])
   extends Decl {
  def params: Seq[Seq[ValDef]] = Nil
}

case class FuncCall(
    receiver: Term,
    function: Term,
    params: Seq[Seq[Sym]])
  extends AST

case class InfixChain(
    syms: Seq[Sym],
    ops:  Seq[Term])
  extends AST



// Named function
// def x[T<:Num](v: String, y: T): String = { v + y.toString }
//
// Decl(Prefix.DEF, Some("x"),
//
// Equivalent to
// val x: [T<:Num](v: String, y: T): String = { v + y.toString }
// Or
// val x: [T<:Num](String, T): String = { $1 + $2.toString }
// Or
// val x = {(v: String, y: String) => v + y }
//
// Mutable
// var def(v: String, y: String): String = { v + y }
// var val x: (v: String, y: String): String = { v + y }

//var def x(v: String=>String = {v => v + " "}, m: String): String = v(m)
//var def x(v(t: String): String = {t + " "}, m: String): String = v(m)
//val y = 32
//var m = 32