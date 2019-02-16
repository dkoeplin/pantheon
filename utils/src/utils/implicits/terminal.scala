package utils.implicits

import utils.PrettyPrinter
import java.io.PrintStream

object terminal {
  val ERROR = s"[${Console.RED}error${Console.RESET}] "
  val WARN  = s"[${Console.YELLOW}warn${Console.RESET}] "
  val BUG   = s"[${Console.MAGENTA}bug${Console.RESET}] "
  val INFO  = s"[${Console.BLUE}info${Console.RESET}] "

  implicit class PrintReport(out: PrintStream) {
    lazy val log   = PrettyPrinter("", out)
    lazy val warn  = PrettyPrinter(WARN, out)
    lazy val error = PrettyPrinter(ERROR, out)
    lazy val bug   = PrettyPrinter(BUG, out)
    lazy val info  = PrettyPrinter(INFO, out)
  }
}
