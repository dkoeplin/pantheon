package utils.implicits

import utils.Printers
import java.io.PrintStream

object terminal {
  val ERROR = s"[${Console.RED}error${Console.RESET}]"
  val WARN  = s"[${Console.YELLOW}warn${Console.RESET}]"
  val BUG   = s"[${Console.MAGENTA}bug${Console.RESET}]"
  val INFO  = s"[${Console.BLUE}info${Console.RESET}]"

  implicit class PrintReport(out: PrintStream) {
    lazy val log = new Printers("", out)
    lazy val warn = new Printers(WARN, out)
    lazy val error = new Printers(ERROR, out)
    lazy val bug = new Printers(BUG, out)
    lazy val info = new Printers(INFO, out)
  }
}
