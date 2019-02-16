package ichor.ast

import ichor.core.Config

case class CodePrinter(cfg: Config) extends Traversal {

  override def visit(node: AST): Unit = node match {
    case Const(lit) => cfg.lnb(lit.toCode)
    case Scope(bracketed, stms) =>
      if (bracketed) cfg.lopen("{") else cfg.lopen()
      stms.foreach{stm =>
        visit(stm)
        cfg.log("")
      }
      if (bracketed) cfg.lclose("}") else cfg.lclose()

    case Bind(term, typ)  => cfg.lnb(s"($term @ $typ)")
    case Switch(cases) =>
      cfg.lopen("{")
      cases.foreach{case Case(cond, body) =>
        cfg.lnb("| ")
        visit(cond)
        cfg.lopen(":")
        visit(body)
        cfg.lclose()
      }
      cfg.lclose("}")

    case FuncDef(tp, name, params, body) =>
      cfg.lnb(s"${tp.toCode} ${name.toCode} (")
      params.zipWithIndex.foreach{case (p, i) =>
        visit(p)
        if (i != params.length - 1) cfg.lnb(", ")
      }
      cfg.lopen(")")
      if (body.isDefined) {
        cfg.lopen(" = {")
        visit(body.get)
        cfg.lclose("}")
      }

    case FuncCall(recip, name, params) =>
      if (recip.isDefined) cfg.lnb(s"${recip.get}.")
      cfg.lnb(name.toCode)
      if (params.isDefined) visit(params.get)

    case Assign(lhs, rhs) =>
      cfg.lnb(s"${lhs.toCode} = ")
      visit(rhs)

    case ValDef(tp, name, rhs) =>
      cfg.lnb(s"${tp.toCode} ${name.toCode}")
      if (rhs.isDefined) {
        cfg.lnb(" = ")
        visit(rhs.get)
      }

    case Tuple(elems) =>
      cfg.lnb("(")
      elems.foreach{e =>
        visit(e)
        cfg.lnb(", ")
      }
      cfg.lnb(")")

  }

}
