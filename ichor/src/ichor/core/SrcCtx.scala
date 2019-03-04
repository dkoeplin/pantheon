package ichor.core

case class SrcCtx(file: String, line: Int, col: Int, content: String)
   extends Meta[SrcCtx](SetBy.Flow.Self)
      with utils.Ctx
{
  override def toString: String = s"$file:$line:$col"
}

object SrcCtx {
  def empty: SrcCtx = SrcCtx("<unknown>", 0, 0, "")
}