package eos.test.ast

import eos.ast.Expressions

import utest._
import ichor.ast._
import ichor.core._

object ExpressionsSuite extends TestSuite with ASTTests { val tests = Tests{
  import eos.ast.implicits._

  val config = new Config
  val graph  = new Graph(config)
  val parser = new Expressions { val IR: Graph = graph }

  'Int - {
    val input = "322"
    val expect = 322
    parse_check(parser.Stm(_))(input, expect)
  }

  'ValRef - {
    val input  = "test_32"
    val expect = "test_32"
    val result = parse_check(parser.Stm(_))(input, expect)
    graph.info(result.ctx)
  }

  'ValDef1 - {
    val input  = "val m32_52"
    val expect = Ref(ValDef(Prefix.VAL,"m32_52",None,None))
    val result = parse_check(parser.Stm(_))(input, expect)

    require(result.getName.contains("m32_52"), s"Result had name ${result.getName}. Expected m32_52.")
    require(result.ctx.toString == "<unknown>:1:5", s"Result had line ${result.ctx}. Expected <unknown>:1:5.")
    graph.info(result.ctx, "val declaration found here.")
    graph.info(result.ctx)
  }

  'ValDefRhs - {
    val input  = "val mXXX = { test } "
    val expect = Ref(ValDef(Prefix.VAL,"mXXX",None,Block(Ref("test"))))
    val result = parse_check(parser.Stm(_))(input, expect)

    require(result.getName.contains("mXXX"), s"Result had name ${result.getName}. Expected mXXX.")
    require(result.ctx.toString == "<unknown>:1:5", s"Result had line ${result.ctx}. Expected <unknown>:1:5.")
    graph.info(result.ctx, "val declaration found here.")
    graph.info(result.ctx)
  }

  'ArgChain - {
    val input  = "(32, 16, two)"
    val expect = Seq(Seq[Sym](32, 16, "two"))
    val result = parse_check(parser.Arg(1)(_))(input, expect)
    graph.info(result)
  }

  'Apply - {
    val input  = "testFunc32(a_16, blah)"
    val expect = Ref(FuncCall("testFunc32", "apply", Seq(Seq[Sym]("a_16", "blah"))))
    val result = parse_check(parser.Stm(_))(input, expect)
    graph.info(result.ctx)
  }

  'Infix - {
    val input  = "mX.funcCall(32, 32)"
    val expect = Ref(FuncCall("mX","funcCall",Seq(Seq[Sym](32,32))))
    val result = parse_check(parser.Stm(_))(input, expect)
    graph.info(result.ctx)
  }

  'InfixChain - {
    val input  = "x + y + z / 23"
    val expect = Ref(InfixChain(Seq[Sym]("x","y","z",23),
                                Seq[Term]("+","+","/")))
    val result = parse_check(parser.Stm(_))(input, expect)
    graph.info(result.ctx)
  }

}}
