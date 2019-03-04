package ichor.ast
import scala.util.parsing.input.Positional

sealed abstract class AST extends Product with Positional

case class Bind(term: Term, typ: Type) extends AST

case class Scope(bracketed: Boolean, stms: Seq[AST]) extends AST { override def toString: String = "<scope>" }

case class Case(cond: AST, body: Scope)
case class Switch(cases: Seq[Case]) extends AST

case class Const(token: Literal) extends AST
case class Tuple(elems: Seq[AST]) extends AST

case class FuncDef(tp: Type, name: Term, params: Seq[ValDef], rhs: Option[AST]) extends AST

case class FuncCall(recip: Option[AST], name: Term, parameters: Option[Tuple]) extends AST

case class Assign(name: Term, rhs: AST) extends AST
case class ValDef(tp: Type, name: Term, rhs: Option[AST]) extends AST