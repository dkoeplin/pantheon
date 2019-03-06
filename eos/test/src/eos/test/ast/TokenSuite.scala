package eos.test.ast

import eos.ast.Tokens
import utest._

object TokenSuite extends TestSuite with ASTTests { val tests = Tests{
  import eos.ast.implicits._

  val parser = new Tokens{ }

  'Hex - {
    val input  = "0x32u"
    val expect = 0x32.u
    parse_check(parser.Number(_))(input, expect)
  }

  "Int" - {
    val input = "321290"
    val expect = 321290
    parse_check(parser.Number(_))(input, expect)
  }

  'Float - {
    val input  = "32.5e15f"
    val expect = 32.5e15f
    parse_check(parser.Number(_))(input, expect)
  }

  'Terms - {
    val input  = "__aadk_ried32__"
    val expect = "__aadk_ried32__"
    parse_check(parser.Term(_))(input, expect)
  }

}}
