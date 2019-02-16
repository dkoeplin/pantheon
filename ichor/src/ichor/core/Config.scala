package ichor.core

import java.io.File

import utils.Printer
import utils.implicits.terminal.{BUG, ERROR, INFO, WARN}

class Config {
  val CWD: String = new File(".").getAbsolutePath

  var logDir: String = CWD + "/log/"
  var genDir: String = CWD + "/out/"

  var enError: Boolean = true
  var enWarn: Boolean = true
  var enInfo: Boolean = true
  var enDbg: Boolean = true
  var enLog: Boolean = true

  val log: Printer   = new Printer("", enLog)
  val dbg: Printer   = new Printer("", enDbg)
  val gen: Printer   = new Printer("", true)
  val info: Printer  = new Printer(INFO, enInfo)
  val warn: Printer  = new Printer(WARN, enWarn).withStream(Console.out)
  val error: Printer = new Printer(ERROR, enError).withStream(Console.out)
  val bug: Printer   = new Printer(BUG, enError).withStream(Console.out)

}
