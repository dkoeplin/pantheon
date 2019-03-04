package ichor.ast

import scala.util.parsing.input.Positional

sealed abstract class Token extends Positional {
  def chars: String
  def toCode: String = chars
  def toAST: String
}

sealed abstract class Literal extends Token

case class NumericLiteral(chars: String) extends Literal {
  def toAST = s"NumericLiteral($chars)"
}
case class StringLiteral(chars: String) extends Literal {
  override def toCode: String = "\"" + chars + "\""
  override def toAST: String = s"StringLiteral($toCode)"
}

case class Keyword(chars: String) extends Token {
  override def toString: String = chars
  override def toAST: String = s"Keyword($chars)"
}
case class Type(chars: String) extends Token {
  override def toString: String = chars
  override def toAST: String = s"Type($chars)"
}
case class Term(chars: String) extends Token {
  override def toString: String = chars
  override def toAST: String = s"Term($chars)"
}

/** An error occurring during lexical analysis. */
case class ErrorToken(msg: String) extends Token {
  def chars: String = msg
  override def toString: String = msg
  override def toCode: String = "<error>"
  override def toAST: String = s"<error: $msg>"
}

/** A class for end-of-file tokens */
case object EOF extends Token {
  def chars = "<eof>"
  override def toAST: String = "<EOF>"
}
