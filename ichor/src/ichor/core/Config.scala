package ichor.core

import java.io.File

class Config {
  val CWD: String = new File(".").getAbsolutePath

  var logDir: String = CWD + "/log/"
  var genDir: String = CWD + "/out/"

  /** Enables common subexpression elimination. */
  var enCSE: Boolean = true

  /** Enables code motion out of blocks when loop invariant. */
  var enCodeMotion: Boolean = true

  var enError: Boolean = true
  var enWarn: Boolean = true
  var enInfo: Boolean = true
  var enDbg: Boolean = true
  var enLog: Boolean = false

}
