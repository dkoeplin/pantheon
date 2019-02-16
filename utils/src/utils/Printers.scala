package utils

import java.io.PrintStream

class Printers(prefix: String, stream: PrintStream) {
  def apply(x: => Any): Unit = stream.println(s"$prefix$x")
  def apply(ctx: Ctx, x: => Any): Unit = stream.println(s"$prefix$ctx: $x")
  def apply(ctx: Ctx, showCaret: Boolean = true): Unit = if (ctx.content.isDefined) {
    stream.println(s"$prefix${ctx.content.get}")
    if (showCaret) stream.println(prefix + " "*(ctx.column-1) + "^") else stream.println("")
  }
}
