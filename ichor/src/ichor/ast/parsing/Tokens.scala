package ichor.ast.parsing

import fastparse._
import fastparse.NoWhitespace._
import ichor.ast.{Term, Unbound}
import ichor.core.{Const, SrcCtx, Type}

trait Tokens {
  def keywords[_:P]: P[Unit]
  def keyops[_:P]: P[Unit]
  val opChars: Set[Char] = Set(
    '!', '#', '%', '&', '*', '+', '-', '/',
    ':', '<', '=', '>', '?', '@', '\\', '^', '|', '~'
  )
  val isOpChar: Char => Boolean = {c => c.isLetterOrDigit || opChars.contains(c)  }
  val isIdChar: Char => Boolean = {c => c.isLetterOrDigit || c == '_' }
  val isFirstIdChar: Char => Boolean = {c => c.isLetter || c == '_' }
  val intChars: Set[Char]
  val fpChars: Set[Char]
  def prefixes[_:P]: P[Unit]
  final lazy val numChars: Set[Char] = intChars ++ fpChars

  def space[_:P]: P[Unit] = P(NoTrace(CharsWhileIn("\u0020\u0009")))

  def literal(c: String, suffix: Char): Const
  def prefixed(prefix: String, c: String, suffix: Char): Const

  def sign[_:P]: P[Unit] = P( CharIn("+\\-") )
  def digits[_:P]: P[Unit] = P( CharsWhileIn("0-9") )
  def frac[_:P]: P[Unit] = P( "." ~ digits )
  def exp[_:P]: P[Unit] = P( CharIn("eE") ~ sign.? ~ digits )
  def fpNum[_:P]: P[Unit] = P( CharPred(CharIf(fpChars.contains)) )
  def intNum[_:P]: P[Unit] = P( CharPred(CharIf(intChars.contains)) )
  def anyNum[_:P]: P[Unit] = P( CharPred(CharIf(numChars.contains)) )

  def LiteralNum[_:P]: P[Const] = P( ( sign.? ~ digits ~ (anyNum | (frac.? ~ exp.? ~ fpNum.?)).?).!.map{
    case v if numChars.contains(v.last) => literal(v.dropRight(1), v.last)
    case v => literal(v, ' ')
  })

  def LiteralHex[_:P]: P[Const] = P( (prefixes.! ~~ (CharsWhileIn("0-9a-fA-F") ~ intNum.?).!).map{
    case (prefix, v) if numChars.contains(v.last) => prefixed(prefix, v.dropRight(1), v.last)
    case (prefix, v) => prefixed(prefix, v, ' ')
  })

  def Number[_:P]: P[Const] = P( LiteralHex | LiteralNum )

  lazy val OpChar = CharIf(isOpChar)
  lazy val IdChar = CharIf(isIdChar)

  def KeyWord[_:P]: P[Unit]
    = P{ keywords ~ !CharPred(IdChar) }
      .opaque("keyword")

  def KeyOp[_:P]: P[Unit]
    = P{ keyops ~ !CharPred(OpChar) }
      .opaque("keyop")

  def Id[_:P]: P[Unit]
    = P{ !KeyWord ~ CharPred(CharIf(isFirstIdChar)) ~ CharsWhile(isIdChar,0) }
      .opaque("id")

  def Op[_:P]: P[Unit]
    = P{ !KeyOp ~ (!StringIn("/*", "//") ~ CharsWhile(isOpChar)) }
      .opaque("op")

  def OpId[_:P]: P[Term] = P{ Op.! }.map{name => new Term(name) at ctx }
  def Term[_:P]: P[Term] = P{ Id.! }.map{name => new Term(name) at ctx }
  def Typ[_:P]: P[Type] = { Id.! }.map{name => new Unbound(name) at ctx }


  def ctx[_:P]: SrcCtx = {
    val file = ParsingRun.current.misc.getOrElse("file", "<unknown>").toString
    val data = ParsingRun.current.input.asInstanceOf[IndexedParserInput].data
    val lookup = if (!ParsingRun.current.misc.contains("lookup")) {
      val look = fastparse.internal.Util.lineNumberLookup(data)
      ParsingRun.current.misc += "lookup" -> look
      look
    }
    else {
      ParsingRun.current.misc("lookup").asInstanceOf[Array[Int]]
    }
    val index = ParsingRun.current.index

    val lookupIndex = lookup.indexWhere(_ > index)
    val line = lookupIndex match{
      case -1 => lookup.length - 1
      case n => math.max(0, n - 1)
    }
    val lineEndIndex =
      if (lookup.isDefinedAt(line + 1)) lookup(line + 1)
      else data.length-1

    val col = index - lookup(line)
    val content = data.slice(lookup(line), lineEndIndex+1)
    SrcCtx(file, line+1, col+1, content)
  }

  object Key {
    def W[_: P](s: String): P[Unit] = P( s ~ !CharPred(isIdChar) )(s"`$s`", implicitly)
    // Don't include comments as part of operators
    def O[_: P](s: String): P[Unit] = P( s ~ (!CharPred(OpChar) | &(NoTrace(StringIn("/*", "//")))) )(s"`$s`", implicitly)
  }
}
