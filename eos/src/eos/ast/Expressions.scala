package eos.ast

import ichor.ast._
import ichor.core._
import fastparse._
import fastparse.ScalaWhitespace._

trait Expressions extends Tokens {
  val IR: Graph
  def Apply = new Term("apply")

  def Brackets[_:P]: P[Block] = P{ "{" ~ Exp.rep ~ "}" }.map{syms =>
    IR.scope {
      syms.lastOption.getOrElse(new Const((), Unk))
    }
  }
  def SingleExp[_:P]: P[Block] = P{ Exp }.map{sym => IR.scope{ IR.add(sym) }}

  def Block[_:P]: P[Block] = P( Brackets | SingleExp )


  /** A simple reference to a term. */
  def Use[_:P]: P[Sym] = P{ Term }.map{t => new Ref(t) named t.name at t.ctx }

  def ValDef[_:P]: P[Sym] = P{ (Key.W("val") | Key.W("var")).! ~ ArgDef}.map{
    case ("val", df) => new Ref(df) named df.term.name at df.term.ctx
    case ("var", df) => new Ref(df) named df.term.name at df.term.ctx
    case _ => throw new Exception("Unreachable")
  }

  def ArgDef[_:P]: P[ValDef] = P{ Term ~ (Key.O("=") ~ Block).? }.map{case (term, rhs) =>
    new ValDef(Prefix.VAL, term, None, rhs)
  }
  def DefDef[_:P]: P[Sym] = P{ Key.W("def") ~ Term ~ ("(" ~ ArgDef.rep ~ ")").rep ~ (Key.O("=") ~ Block).? }.map{
    case (term, params, rhs) => new Ref(new DefDef(Prefix.DEF, term, params, None, rhs)) named term.name at term.ctx
  }

  def Arg[_:P](n: Int): P[Seq[Seq[Sym]]] = P{ ("(" ~/ Exp.rep(0, ",") ~ ")").rep(n) }

  def ApplyFunc[_:P]: P[Sym] = P{ Term ~ Arg(1) }.map{case (r, params) =>
    new Ref(FuncCall(r, Apply at r.ctx.copy(col=r.ctx.col+r.name.length), params)) at r.ctx
  }
  def DotInfixFunc[_:P]: P[Sym] = P{Term ~ "." ~/ OpId ~ Arg(0) }.map{
    case (r, func, params) => new Ref(FuncCall(r, func, params)) at r.ctx
  }

  def Exp[_:P]: P[Sym] = P(
      ValDef
    | DefDef
    | ApplyFunc
    | DotInfixFunc
    | Use
  )

}
