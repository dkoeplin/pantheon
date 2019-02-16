package utils.io

import java.io.PrintStream

object NullPrintStream extends PrintStream(NullOutputStream)
