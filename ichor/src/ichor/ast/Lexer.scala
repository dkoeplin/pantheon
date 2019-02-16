package ichor.ast
import java.io.{BufferedReader, FileReader}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.util.parsing.input.{Position, Reader, StreamReader}

class Lexer extends Parsers {
  override type Elem  = Char
  type Token = ichor.ast.Token

  /** The set of reserved identifiers: these will be returned as `Keyword`s. */
  val keywords: Set[String] = Set()

  /** The set of delimiters (ordering does not matter). */
  val delimiters: Set[String] = Set(":", "|", "{", "}", "(", ")", "=", ",", "@", "?", ".")
  lazy val delim: Parser[Token] = {
    // Sort delimeters longest to shortest so that longest are chosen first
    delimiters.toArray.sortBy{d => -d.length }
      .map{d => accept(d.toList) ^^ {_ => Keyword(d) }}
      .reduce{_ | _}
  }

  val single: Parser[Elem] = elem("", _ == '\'')

  /** Creates a lexer matching any character except the ones given in `cs` (and returns it).*/
  def chrExcept(cs: Char*): Parser[Elem] = elem("", ch => !cs.contains(ch))

  /** Captures a character which can be used in a string literal enclosed by double quotes. */
  val doubleQs: Parser[Elem] = chrExcept('\"', '\n', EofCh)
  /** Captures a character which can be used in a string literal enclosed by single quotes. */
  val singleQs: Parser[Elem] = chrExcept('\'', '\n', EofCh)
  /** Captures a white-space character. */
  val whitespaceChar: Parser[Elem] = elem("whitespace", ch => ch <= ' ' && ch != EofCh)
  /** Captures a digit. */
  val digit: Parser[Elem] = elem("digit", _.isDigit)
  /** Captures a letter. */
  val letter: Parser[Elem]    = elem("letter", _.isLetter)
  /** Captures an uppercase letter. */
  val uppercase: Parser[Elem] = elem("uppercase", _.isUpper)
  /** Captures a lowercase letter. */
  val lowercase: Parser[Elem] = elem("lowercase", _.isLower)
  /** Captures characters which can be used in identifiers. */
  val identChar: Parser[Elem] = letter | digit | elem('_')

  val specialFuncChars: Set[Elem] = Set(':')
  val sfc: Parser[Elem] = specialFuncChars.map{d => accept(d) }.reduce{_ | _}

  val prefixChars: Set[Elem] = Set('!', '+', '-', '~')
  val pc: Parser[Elem] = prefixChars.map{d => accept(d) }.reduce(_ | _)

  val firstFuncChars: Set[Elem] = Set('!', '%', '^', '&', '*', '+', '-', '=', '<', '>', '/', '|', '~')
  /** Captures a character which can be used in any symbolic function name. */
  val fc: Parser[Elem] = firstFuncChars.map{d => accept(d) }.reduce{_ | _}

  val afc: Parser[Elem] = fc | sfc

  val dot: Parser[Elem] = accept('.')

  val symbolicFuncName: Parser[Token] = (
    fc ~ fc.*  ^^ {case first ~ rest => Term(first :: rest mkString "") }
      //| fc ~ fc.+ ^^ {case first ~ rest => Term(first :: rest mkString "") }
      | dot ~ dot   ^^ {_ => Term("..") }
    )

  def identifier(chars: String): Token = if (keywords.contains(chars)) Keyword(chars) else Term(chars)

  val term: Parser[Token] = (
    lowercase ~ rep(identChar) ~ opt(single) ^^ {case first ~ rest ~ s => identifier((first :: rest ++ s) mkString "") }
      | symbolicFuncName
    )

  val typ: Parser[Type]
  = uppercase ~ rep(identChar) ^^ { case first ~ rest => Type(first :: rest mkString "") }

  val whitespace: Parser[Any] = rep[Any](
    whitespaceChar
      | '/' ~ '*' ~ comment
      | '/' ~ '/' ~ rep( chrExcept(EofCh, '\n') )
      | '/' ~ '*' ~ failure("unclosed comment")
  )

  val integer: Parser[NumericLiteral] = digit.+ ^^ { digits => NumericLiteral(digits mkString "") }
  val string: Parser[StringLiteral] = (
    '\"' ~> doubleQs.* <~ '\"' ^^ { chars => StringLiteral(chars mkString "") }
      | '\'' ~> singleQs.* <~ '\'' ^^ { chars => StringLiteral(chars mkString "") }
    )

  def comment: Parser[Any] = (
    rep (chrExcept (EofCh, '*')) ~ '*' ~ '/'     ^^ { _ => ' ' }
      | rep (chrExcept (EofCh, '*')) ~ '*' ~ comment ^^ { _ => ' ' }
    )

  val token: Parser[Token] = (
    delim
      | term
      | typ
      | integer
      | string
      | EofCh ^^^ EOF
      | '\'' ~> failure("unclosed string literal")
      | '\"' ~> failure("unclosed string literal")
      | failure("illegal character")
    )

  /** A reader that produces `Token`s from a stream of characters. */
  class Scanner(file: String, in: Reader[Char]) extends Reader[Token] {
    def this(file: String) = this(file, StreamReader(new BufferedReader(new FileReader(file))))

    /** Convenience constructor (makes a character reader out of the given string) */
    //def this(in: String) = this(new CharArrayReader(in.toCharArray))

    private val (tok, rest1, rest2) = whitespace(in) match {
      case Success(_, next) => token(next) match {
        case Success(t, in2) => (t, next, in2)
        case ns: NoSuccess   => (ErrorToken(ns.msg), ns.next, skip(ns.next))
      }
      case ns: NoSuccess     => (ErrorToken(ns.msg), ns.next, skip(ns.next))
    }
    private def skip(in: Reader[Char]): Reader[Char] = if (in.atEnd) in else in.rest

    override def source: java.lang.CharSequence = in.source
    override def offset: Int = in.offset
    def first: Token = tok
    def rest = new Scanner(file, rest2)
    def pos: Position = rest1.pos
    def atEnd: Boolean = in.atEnd || (whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })

    override def toString: String = s"$file:${pos.line}:${pos.column}"
  }

  def scanner(file: String): Scanner = new Scanner(file)
}