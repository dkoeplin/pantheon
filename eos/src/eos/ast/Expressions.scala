package eos.ast

import ichor.ast._
import ichor.core._
import fastparse._
import fastparse.ScalaWhitespace._

trait Expressions extends Tokens {
  val IR: Graph
  def Apply: Term = new Term("apply")

  def BlockBrackets[_:P]: P[Block] = P{ "{" ~ Stm.rep ~ "}" }.map{syms =>
    IR.scope {
      syms.foreach{sym => IR.add(sym) }
      syms.lastOption.getOrElse(new Const((), Unk))
    }
  }
  def BlockSingle[_:P]: P[Block] = P{ ComplexExp }.map{sym =>
    IR.scope{ IR.add(sym) }
  }

  def Block[_:P]: P[Block] = P( BlockBrackets | BlockSingle )

  def BlockExp[_:P]: P[Sym] = P{ BlockBrackets }.map{block =>
    new Ref(new BlockExp(block)) at ctx
  }


  /** A simple reference to a term. */
  def Use[_:P]: P[Sym] = P{ Term }.map{t => new Ref(t) named t.name at t.ctx }

  def ValDef[_:P]: P[Sym] = P{ (Val | Var) ~ ArgDef}.map{case (prefix, df) =>
    new Ref(df.copy(prefix=prefix)) named df.term.name at df.term.ctx
  }

  def ArgDef[_:P]: P[ValDef] = P{ Term ~ (Key.O("=") ~ Block).? }.map{
    case (term, rhs) => new ValDef(Prefix.VAL, term, None, rhs)
  }
  def DefDef[_:P]: P[Sym] = {
    P{
      Def ~ Term ~ ("(" ~ ArgDef.rep ~ ")").rep ~
      (Key.O("=") ~ Block).?
    }.map{case (prefix, term, params, rhs) =>
      new Ref(new DefDef(prefix, term, params, None, rhs))
        .named(term.name)
        .at(term.ctx)
    }
  }

  def Arg[_:P](n: Int): P[Seq[Seq[Sym]]]
    = P{ ("(" ~/ ComplexExp.rep(0, ",") ~ ")").rep(n) }

  def ApplyFunc[_:P]: P[Sym] = P{ Term ~ Arg(1) }.map{case (r, params) =>
    val ctx = r.ctx.copy(col=r.ctx.col+r.name.length)
    new Ref(FuncCall(r, Apply at ctx, params)) at ctx
  }
  def DotInfixFunc[_:P]: P[Sym] = P{Term ~ "." ~/ OpId ~ Arg(0) }.map{
    case (r, func, params) => new Ref(FuncCall(r, func, params)) at func.ctx
  }
  def InfixChain[_:P]: P[Sym] = {
    P{SimpleExp ~ (OpId ~ SimpleExp).rep }.map{
      case (first, Nil)   => first
      case (first, calls) =>
        new Ref(new InfixChain(first +: calls.map(_._2), calls.map(_._1)))
    }
  }

  def SimpleExp[_:P]: P[Sym] = P(
      ApplyFunc
    | DotInfixFunc
    | BlockExp
    | Use
    | Number
  ).opaque("expression")

  def ComplexExp[_:P]: P[Sym] = P(
    InfixChain
  ).opaque("expression")

  def Stm[_:P]: P[Sym] = P(
      ValDef
    | DefDef
    | ComplexExp
  ).opaque("statement")

}
