package ichor.ast

import ichor.core.Graph

case class CodePrinter(IR: Graph) extends Traversal {
  import IR._

  override def visit(node: AST): Unit = node match {
    case Const(lit) => dbg.raw(lit.toCode)
    case Scope(bracketed, stms) =>
      val bracket = if (bracketed) "{" else ""
      dbg.tab(bracket){
        stms.foreach{stm =>
          visit(stm)
          dbg("")
        }
      }
      if (bracketed) dbg("}") else dbg("")

    case Bind(term, typ)  => dbg.raw(s"($term @ $typ)")
    case Switch(cases) =>
      dbg.tab("{"){
        cases.foreach{case Case(cond, body) =>
          dbg.raw("| ")
          visit(cond)
          dbg.tab(":"){
            visit(body)
          }
        }
      }
      dbg("}")

    case FuncDef(tp, name, params, body) =>
      dbg.raw(s"${tp.toCode} ${name.toCode} (")
      params.zipWithIndex.foreach{case (p, i) =>
        visit(p)
        if (i != params.length - 1) dbg.raw(", ")
      }
      dbg(")")
      if (body.isDefined) {
        dbg.tab(" = {"){
          visit(body.get)
        }
        dbg("}")
      }

    case FuncCall(recip, name, params) =>
      if (recip.isDefined) dbg.raw(s"${recip.get}.")
      dbg.raw(name.toCode)
      if (params.isDefined) visit(params.get)

    case Assign(lhs, rhs) =>
      dbg.raw(s"${lhs.toCode} = ")
      visit(rhs)

    case ValDef(tp, name, rhs) =>
      dbg.raw(s"${tp.toCode} ${name.toCode}")
      if (rhs.isDefined) {
        dbg.raw(" = ")
        visit(rhs.get)
      }

    case Tuple(elems) =>
      dbg.raw("(")
      elems.foreach{e =>
        visit(e)
        dbg.raw(", ")
      }
      dbg.raw(")")

  }

}
