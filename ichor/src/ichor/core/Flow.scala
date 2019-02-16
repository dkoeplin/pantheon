package ichor.core

class Flow(name: String, func: PartialFunction[(Ref,Def), Unit]) {
  def apply(ref: Ref, df: Def): Unit = if (func.isDefinedAt((ref,df))) func((ref,df))
}

object Flow {
  implicit class SymbolOps(x: Symbol) {
    def apply(func: PartialFunction[(Ref,Def), Unit]): Flow = new Flow(x.name, func)
  }
}
