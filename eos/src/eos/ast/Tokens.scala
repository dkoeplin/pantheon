package eos.ast

import ichor.core.Const
import eos.types._
import fastparse._

trait Tokens extends ichor.ast.parsing.Tokens {

  def keywords[_:P]: P[Unit] = P{ StringIn("true", "false") }
  def keyops[_:P]: P[Unit] = P{ StringIn(":") }
  override val intChars: Set[Char] = Set('u', 'U', 'L')
  override val fpChars: Set[Char] = Set('f', 'd', 'h', 'b')
  def prefixes[_:P]: P[Unit] = P{ StringIn("0x") }

  override def literal(v: String, suffix: Char): Const = suffix match {
    case 'u' => new Const(v.toInt, U32)
    case 'U' => new Const(v.toLong, U32)
    case 'L' => new Const(v.toLong, U32)
    case 'f' => new Const(v.toFloat, FP32)
    case 'd' => new Const(v.toDouble, FP64)
    case 'h' => new Const(v.toFloat, FP16)
    case 'b' => new Const(v.toFloat, BF16)
    case  _  => new Const(v.toInt, I32)
  }

  override def prefixed(prefix: String, v: String, suffix: Char): Const = prefix match {
    case "0x" => suffix match {
      case 'u' => new Const(java.lang.Integer.parseInt(v, 16), U32)
      case 'U' => new Const(java.lang.Long.parseLong(v, 16), U64)
      case 'L' => new Const(java.lang.Long.parseLong(v, 16), I64)
      case  _  => new Const(java.lang.Integer.parseInt(v, 16), I32)
    }
    case _ => throw new Exception("Unreachable") // Unreachable
  }

}
