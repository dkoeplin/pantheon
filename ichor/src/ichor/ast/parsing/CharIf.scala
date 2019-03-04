package ichor.ast.parsing

case class CharIf(f: Char => Boolean)(implicit name: sourcecode.Name) extends (Char => Boolean){
  def apply(t: Char): Boolean = f(t)
  override def toString(): String = name.value
}
