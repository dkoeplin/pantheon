package utils

import java.io.{FileOutputStream, PrintStream}
import scala.util.parsing.input.Position

import utils.io.NullPrintStream

class Printer(pre: String, enable: => Boolean) extends PrettyPrinter {
  var filename: Option[String] = None
  var stream: PrintStream = NullPrintStream
  var prefix: String = pre

  var count: Int = 0

  override def apply(x: => Any): Unit = if (enable) super.apply(x)
  override def apply(ctx: Position, x: => Any): Unit = if (enable) super.apply(ctx,x)

  /** Print source context content. Increment count for this printer if showCaret is true. */
  override def apply(ctx: Position): Unit = {
    if (enable) super.apply(ctx)
    count += 1
  }

  /** Open a tabbed scope with the given prefix line. */
  def tab[A](x: => Any)(func: => A): A = {
    val savePrefix = prefix
    apply(x)
    prefix = "  "+prefix
    val result = func
    prefix = savePrefix
    result
  }

  /** Enter a scope with this printer changing its output to the specified file.
    *
    * Upon entering the scope, the current stream will be saved. If an associated filename exists, the
    * current stream will also be closed.
    * Upon exiting the scope, the file will be closed for writing and the previous stream will be re-opened to
    * append if it was closed.
    */
  def in[A](dir: String, file: String)(func: => A): A = {
    val saveFile   = filename
    val saveStream = stream
    if (filename.isDefined) stream.close()
    stream = new PrintStream(new FileOutputStream(dir + "/" + file, true))
    val result = func
    stream.close()
    stream = saveFile.map{name => new PrintStream(new FileOutputStream(name, true)) }.getOrElse(saveStream)
    result
  }

  def withStream(s: PrintStream): Printer = { stream = s; this }

}
