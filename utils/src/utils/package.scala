package object utils {

  def escapeString(raw: String): String = "\"" + raw.flatMap(escapeChar) + "\""
  def escapeChar(raw: Char): String = raw match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case c    if c.isControl => "\\0" + Integer.toOctalString(c.toInt)
    case c    => String.valueOf(c)
  }

  def plural(x: Int, sing: String): String = if (x == 1) sing else sing+"s"
  def plural(x: Int, sing: String, plur: String): String = if (x == 1) sing else plur
  def conj(xs: Seq[String]): String = {
    import utils.implicits.collections._

    if (xs.isEmpty) ""
    else if (xs.lengthIs(1)) xs.head
    else if (xs.lengthIs(2)) xs.head + " and " + xs.last
    else xs.dropRight(1).mkString(", ") + ", and " + xs.last
  }

  def escapeConst(x: Any): String = x match {
    case c: String => escapeString(c)
    case c: Char => escapeChar(c)
    case c => c.toString
  }

}
