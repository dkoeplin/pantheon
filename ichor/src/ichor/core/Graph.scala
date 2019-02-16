package ichor.core

class Graph {
  /** Statements in the current scope. Order is most recent to least recent. */
  var scope: Vector[Ref] = Vector.empty

  /** Impure statements in the current scope. Order is most recent to least recent. */
  var impure: Vector[Ref] = Vector.empty

  /** Definition cache used for CSE */
  var cache: Map[Def, Ref] = Map.empty

  /** Flow rules. */
  var flows: Vector[Flow] = Vector.empty


  private def register(lhs: Option[Ref], df: Def, flow: Sym => Unit = _ => ()): Sym = Option(df.rewrite).getOrElse{
    // 2) Calculate effects
    val effects = df.effects.inContext(impure)
    // 3) Attempt to CSE
    if (effects.mayCSE) {
      val cached = cache.get(df).filter(_.effects == effects)
      if (cached.isDefined) return cached.get
    }
    // 4) Register in graph, in CSE cache, and in impure. Set effects
    val ref = lhs.getOrElse(new Ref(df))
    scope :+= ref
    if (effects.mayCSE) cache += df -> ref
    if (!effects.isPure) impure :+= ref
    ref.effects = effects

    // 5) Immediate flow
    flow(ref)

    // 6) Other flows
    flows.foreach{_.apply(ref,df) }

    ref
  }

  /** Add the given operation to the graph.
    * @return a symbol representing the result of the node.
    */
  def add(df: Def): Sym = register(None, df)

  /** Add the given operation to the graph, running the flow rule on the resulting edge.
    * @return a symbol representing the result of the node.
    */
  def addWithFlow(df: Def)(flow: Sym => Unit): Sym = register(None, df, flow)

  def readd(sym: Sym): Sym
    = sym.stm.map{case (ref,df) => register(Some(ref),df) }.getOrElse(sym)

  def readdWithFlow(sym: Sym)(flow: Sym => Unit): Sym
    = sym.stm.map{case (ref,df) => register(Some(ref),df,flow) }.getOrElse(sym)
}
