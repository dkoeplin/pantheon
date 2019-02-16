package ichor.ast

import ichor.core.Graph

case class ASTPrinter(IR: Graph) extends Traversal {
  import IR._

  override def visit(node: AST): Unit = node match {
    case Const(_) => dbg.raw(node)
    case Scope(bracketed, stms) =>
      dbg.tab(s"Scope(bracketed = $bracketed, Seq("){
        stms.foreach{stm =>
          visit(stm)
          dbg.raw(",")
        }
      }
      dbg("))")

    case Bind(term, typ) => dbg.raw(s"Bind($term, $typ)")
    case Switch(cases) =>
      dbg.tab("Switch("){
        cases.zipWithIndex.foreach{case (Case(cond, body), i) =>
          dbg("Case(")
          visit(cond)
          dbg.raw(", ")
          visit(body)
          dbg(")")
          if (i != cases.length - 1) dbg.raw(", ")
        }
      }
      dbg(")")

    case FuncDef(tp, name, params, rhs) =>
      dbg.tab(s"FuncDef("){
        dbg(s"tp     = ${tp.toCode},")
        dbg(s"name   = ${name.toCode},")
        dbg(s"params = Seq(")
        params.zipWithIndex.foreach{case (param,i) =>
          visit(param)
          if (i != params.length - 1) dbg(", ")
        }
      }
      if (rhs.isEmpty) {
        dbg("), rhs = None)")
      }
      else {
        dbg("),")
        dbg.raw("rhs = Some(")
        visit(rhs.get)
        dbg("))")
      }

    case FuncCall(recip, name, params) =>
      dbg.raw("FuncCall(recip = ")
      if (recip.isDefined) {
        visit(recip.get)
      }
      else dbg.raw("None")
      dbg.raw(s" name = $name, params = ")
      if (params.isDefined) {
        dbg.raw("Some(")
        visit(params.get)
        dbg("))")
      }
      else dbg("None)")

    case Assign(lhs, rhs) =>
      dbg.raw(s"Assign(${lhs.toAST}, ")
      visit(rhs)
      dbg(")")

    case ValDef(tp, name, rhs) =>
      dbg.raw(s"ValDef(tp = ${tp.toAST}, name = $name, rhs = ")
      if (rhs.isDefined) {
        dbg.raw("Some(")
        visit(rhs.get)
        dbg("))")
      }
      else dbg.raw("None)")

    case Tuple(elems) =>
      dbg.raw("Tuple(Seq(")
      elems.foreach{elem => visit(elem); dbg.raw(", ") }
      dbg("))")
  }
}