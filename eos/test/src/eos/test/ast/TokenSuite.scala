package eos.test.ast

import eos.ast.Tokens
import utest._
import ichor.core._
import ichor.ast._
import eos.types._
import fastparse._

object TokenSuite extends TestSuite { val tests = Tests{
  val parser = new Tokens{ }

  'Hex - {
    val expect = new Const(0x32, U32)
    val Parsed.Success(s,_) = parse("0x32u", parser.Number(_))
    require(s == expect, s"Parser returned $s. Expected $expect.")
  }

  'Float - {
    val expect = new Const(32.5e15f, FP32)
    val Parsed.Success(s,_) = parse("32.5e15f", parser.Number(_))
    require(s == expect, s"Parser returned $s. Expected $expect.")
  }

  'Terms - {
    val expect = new Term("__aadk_ried32__")
    val Parsed.Success(s,_) = parse("__aadk_ried32__", parser.Term(_))
    require(s == expect, s"Parser returned $s. Expected $expect.")
  }

}}
