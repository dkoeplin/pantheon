package ichor.ast

import java.io.{BufferedReader, FileReader}

import scala.util.parsing.input.{Position, Reader, StreamReader}

/** A reader that produces `Token`s from a stream of characters. */
class Scanner(file: String, in: Reader[Char], lexer: Lexer) extends Reader[Token] {
  def this(file: String, lexer: Lexer) = this(file, StreamReader(new BufferedReader(new FileReader(file))), lexer)

  import lexer.{Success,NoSuccess}

  private val (tok, rest1, rest2) = lexer.whitespace(in) match {
    case Success(_, next) => lexer.token(next) match {
      case Success(t, in2) => (t, next, in2)
      case ns: NoSuccess   => (ErrorToken(ns.msg), ns.next, skip(ns.next))
    }
    case ns: NoSuccess     => (ErrorToken(ns.msg), ns.next, skip(ns.next))
  }
  private def skip(in: Reader[Char]): Reader[Char] = if (in.atEnd) in else in.rest

  override def source: java.lang.CharSequence = in.source
  override def offset: Int = in.offset
  def first: Token = tok
  def rest = new Scanner(file, rest2, lexer)
  def pos: Position = rest1.pos
  def atEnd: Boolean = in.atEnd || (lexer.whitespace(in) match { case Success(_, in1) => in1.atEnd case _ => false })

  override def toString: String = s"$file:${pos.line}:${pos.column}"
}
