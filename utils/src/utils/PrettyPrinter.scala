package utils

import java.io.PrintStream

trait PrettyPrinter {
  def prefix: String
  def stream: PrintStream

  def raw(x: => Any): Unit = stream.print(s"$x")

  /** Print the given line with this Printer's prefix. */
  def apply(x: => Any): Unit = stream.println(s"$prefix$x")

  /** Print the given line with this Printer's prefix and the source context information. */
  def apply(ctx: Ctx, x: => Any): Unit = stream.println(s"$prefix$ctx: $x")

  /** Print the source context content. */
  def apply(ctx: Ctx): Unit = {
    stream.println(s"$prefix${ctx.content}")
    if (ctx.content.nonEmpty) stream.println(s"$prefix${ctx.caret}")
  }
}

object PrettyPrinter {
  def apply(pre: String, stm: PrintStream): PrettyPrinter = new PrettyPrinter {
    def prefix: String = pre
    def stream: PrintStream = stm
  }
}
