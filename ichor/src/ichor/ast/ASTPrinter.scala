package ichor.ast

import ichor.core.Config

case class ASTPrinter(cfg: Config) extends Traversal {

  override def visit(node: AST): Unit = node match {
    case Const(_) => cfg.lnb(node)
    case Scope(bracketed, stms) =>
      cfg.lopen(s"Scope(bracketed = $bracketed, Seq(")
      stms.foreach{stm =>
        visit(stm)
        cfg.log(",")
      }
      cfg.lclose("))")

    case Bind(term, typ) => cfg.lnb(s"Bind($term, $typ)")
    case Switch(cases) =>
      cfg.lopen("Switch(")
      cases.zipWithIndex.foreach{case (Case(cond, body), i) =>
        cfg.lnb("Case(")
        visit(cond)
        cfg.lnb(", ")
        visit(body)
        cfg.lnb(")")
        if (i != cases.length - 1) cfg.lnb(", ")
      }
      cfg.lclose(")")

    case FuncDef(tp, name, params, rhs) =>
      cfg.lopen(s"FuncDef(")
      cfg.log(s"tp     = ${tp.toCode},")
      cfg.log(s"name   = ${name.toCode},")
      cfg.lopen(s"params = Seq(")
      params.zipWithIndex.foreach{case (param,i) =>
        visit(param)
        if (i != params.length - 1) cfg.lnb(", ")
      }
      if (rhs.isEmpty) {
        cfg.lclose("), rhs = None)")
      }
      else {
        cfg.lclose("),")
        cfg.lnb("rhs = Some(")
        visit(rhs.get)
        cfg.lclose("))")
      }

    case FuncCall(recip, name, params) =>
      cfg.lnb("FuncCall(recip = ")
      if (recip.isDefined) {
        visit(recip.get)
      }
      else cfg.lnb("None")
      cfg.lnb(s" name = $name, params = ")
      if (params.isDefined) {
        cfg.lnb("Some(")
        visit(params.get)
        cfg.log("))")
      }
      else cfg.log("None)")

    case Assign(lhs, rhs) =>
      cfg.lnb(s"Assign(${lhs.toAST}, ")
      visit(rhs)
      cfg.log(")")

    case ValDef(tp, name, rhs) =>
      cfg.lnb(s"ValDef(tp = ${tp.toAST}, name = $name, rhs = ")
      if (rhs.isDefined) {
        cfg.lnb("Some(")
        visit(rhs.get)
        cfg.log("))")
      }
      else cfg.lnb("None)")

    case Tuple(elems) =>
      cfg.lnb("Tuple(Seq(")
      elems.foreach{elem => visit(elem); cfg.lnb(", ") }
      cfg.log("))")
  }
}