package ichor.core

import java.io.File

class Config {
  val CWD: String = new File(".").getAbsolutePath

  var logDir: String = CWD + "/log/"
  var genDir: String = CWD + "/out/"

  var enError: Boolean = true
  var enWarn: Boolean = true
  var enInfo: Boolean = true
  var enDbg: Boolean = true
  var enLog: Boolean = false

}
