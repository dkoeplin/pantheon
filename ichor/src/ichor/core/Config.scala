package ichor.core

import java.io.{File, PrintStream}

class Config {
  val CWD: String = new File(".").getAbsolutePath

  private var logtab: Int = 0
  private var logfirst: Boolean = true
  private var logstream: PrintStream = new PrintStream("log")
  private def logspace: String = if (logfirst) "  "*logtab else ""

  var logDir: String = CWD + "/logs/"

  def lnb(x: => Any): Unit = { logstream.print(logspace + x.toString); logfirst = false }
  def log(x: => Any): Unit = { logstream.println(logspace + x.toString); logfirst = true }
  def lopen(): Unit = { logtab += 1 }
  def lopen(x: => Any): Unit = { log(x); logtab += 1 }
  def lclose(): Unit = { logtab -= 1 }
  def lclose(x: => Any): Unit = { logtab -= 1; log(x) }
}
