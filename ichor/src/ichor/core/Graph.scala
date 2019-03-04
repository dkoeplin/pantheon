package ichor.core

import utils.{NullPrinter, Printer}
import utils.implicits.terminal.{BUG, ERROR, INFO, WARN}

import scala.collection.mutable

class Graph(var cfg: Config) {
  type Scope = Block

  /** Current compiler phase */
  var phase: Int = 0

  /** Current compiler pass */
  var pass: Int = 0

  def paddedPass: String = paddedPass(pass)
  def paddedPass(pass: Int): String = { val p = pass.toString; "0"*(4 - p.length) + p }

  /** Statements in the current scope. Order is most recent to least recent. */
  protected val scopes = mutable.Stack.empty[mutable.ArrayBuffer[Ref]]

  /** Impure statements in the current scope. Order is most recent to least recent. */
  protected val impures = mutable.Stack.empty[mutable.ArrayBuffer[Ref]]

  /** Definition cache used for CSE */
  protected var caches = mutable.Stack.empty[mutable.HashMap[Def, Ref]]

  protected def lookupCSE(df: Def): Option[Ref] = if (!cfg.enCSE) None else {
    if (cfg.enCodeMotion) caches.collectFirst{case cache if cache.contains(df) => cache(df) }
    else caches.head.get(df)
  }

  /** Flow rules. */
  protected var flows: Vector[Flow] = Vector.empty


  var log: Printer   = new NullPrinter
  val dbg: Printer   = new Printer("", cfg.enDbg)
  val gen: Printer   = new Printer("", true)
  val info: Printer  = new Printer(INFO, cfg.enInfo).withStream(Console.out)
  val warn: Printer  = new Printer(WARN, cfg.enWarn).withStream(Console.out)
  val error: Printer = new Printer(ERROR, cfg.enError).withStream(Console.out)
  val bug: Printer   = new Printer(BUG, cfg.enError).withStream(Console.out)


  protected def register(lhs: Option[Ref], df: Def, flow: Option[Flow]): Sym = Option(df.rewrite).getOrElse{
    val scope  = scopes.head
    val impure = impures.head
    val cache  = caches.head

    // 2) Calculate effects
    val effects = df.effects.inContext(impure)

    // 3) Attempt to CSE
    if (effects.mayCSE) {
      val cached = lookupCSE(df).filter(_.effects == effects)
      if (cached.isDefined) return cached.get
    }

    // 4) Register in graph, in CSE cache, and in impure. Set effects
    val ref = lhs.getOrElse(new Ref(df))
    scope += ref
    if (effects.mayCSE && cfg.enCSE) cache(df) = ref
    if (!effects.isPure) impure += ref
    ref.effects = effects

    // 5) Immediate flow
    flow.foreach(_.apply(ref,df))

    // 6) Other flows
    flows.foreach{_.apply(ref,df) }

    ref
  }

  /** Add the given operation to the graph.
    * @return a symbol representing the result of the node.
    */
  def add(df: Def): Sym = register(None, df, None)

  /** Add the given operation to the graph, running the flow rule on the resulting edge.
    * @return a symbol representing the result of the node.
    */
  def addWithFlow(df: Def)(flow: Flow): Sym = register(None, df, Some(flow))

  def add(sym: Sym): Sym
    = sym.stm.map{case (ref,df) => register(Some(ref),df, None) }.getOrElse(sym)

  def addWithFlow(sym: Sym)(flow: Flow): Sym
    = sym.stm.map{case (ref,df) => register(Some(ref),df,Some(flow)) }.getOrElse(sym)


  /** Create a scope with no inputs. */
  def scope(func: => Sym): Scope = lambda(Nil)(func)

  /** Create a scope with bound inputs. */
  def lambda(inputs: Seq[Dyn])(func: => Sym): Scope = {
    scopes.push(mutable.ArrayBuffer.empty[Ref])
    impures.push(mutable.ArrayBuffer.empty[Ref])
    if (cfg.enCSE) caches.push(mutable.HashMap.empty[Def,Ref])
    val result = func
    if (cfg.enCSE) caches.pop()
    schedule(inputs, result)
  }


  protected def schedule(inputs: Seq[Dyn], result: Sym): Scope = {
    val scope  = scopes.pop()
    val impure = impures.pop()
    val effects = summzarizeScopeEffects(impure)
    new Block(inputs, scope, result, effects)
  }

}
