package utils

import java.io.PrintStream
import scala.util.parsing.input.Position

trait PrettyPrinter {
  def prefix: String
  def stream: PrintStream

  def raw(x: => Any): Unit = stream.print(s"$x")

  /** Print the given line with this Printer's prefix. */
  def apply(x: => Any): Unit = stream.println(s"$prefix$x")

  /** Print the given line with this Printer's prefix and the source context information. */
  def apply(ctx: Position, x: => Any): Unit = stream.println(s"$prefix$ctx: $x")

  /** Print the source context content. If showCaret is true, include a line after pointing to the column. */
  def apply(ctx: Position): Unit = {
    val lines = ctx.longString.split('\n')
    lines.foreach{line => apply(line) }
  }
}

object PrettyPrinter {
  def apply(pre: String, stm: PrintStream): PrettyPrinter = new PrettyPrinter {
    def prefix: String = pre
    def stream: PrintStream = stm
  }
}
