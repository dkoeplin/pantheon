package eos.test.ast

import eos.ast.Expressions

import utest._
import ichor.ast._
import ichor.core._
import fastparse._

object ExpressionsSuite extends TestSuite { val tests = Tests{
  val config = new Config
  val graph  = new Graph(config)
  val parser = new Expressions { val IR: Graph = graph }

  'ValRef - {
    val input = s"""test_32"""
    val Parsed.Success(s, _) = parse(input, parser.Exp(_))

    val matches = s match {
      case Def(Term("test_32")) => true
      case _ => false
    }

    require(matches, s"Result was not a Term reference.")
    graph.info(s.ctx)
  }

  'ValDef1 - {
    val input = s"""val m32_52"""
    val Parsed.Success(s, _) = parse(input, parser.ValDef(_))

    require(s.getName.contains("m32_52"), s"Result had name ${s.getName}. Expected m32_52.")
    require(s.ctx.toString == "<unknown>:1:5", s"Result had line ${s.ctx}. Expected <unknown>:1:5.")
    graph.info(s.ctx, "val declaration found here.")
    graph.info(s.ctx)
  }

  'ValDefRhs - {
    val input = s"""val mXXX = { test } """
    val Parsed.Success(s, _) = parse(input, parser.ValDef(_))

    require(s.getName.contains("mXXX"), s"Result had name ${s.getName}. Expected mXXX.")
    require(s.ctx.toString == "<unknown>:1:5", s"Result had line ${s.ctx}. Expected <unknown>:1:5.")
    graph.info(s.ctx, "val declaration found here.")
    graph.info(s.ctx)
  }

  'ArgChain - {
    val input = s"""(32, 16, two)"""
    val output = parse(input, parser.Arg(1)(_))
    val matched = output match {
      case Parsed.Success(Seq(Seq(a, b, c)), _) => true
      case x => graph.error(x.toString); false
    }
    require(matched, s"Did not match. Got $output instead.")
  }

  'Apply - {
    val input = s"""testFunc32(a_16, bfefafar)"""
    val output = parse(input, parser.Exp(_))
    val matched = output match {
      case Parsed.Success(Def(FuncCall(Term("testFunc32"), Term("apply"), Seq(Seq(a,b)))),_) => true
      case _ => false
    }
    require(matched, s"Did not match apply function. Got $output")
  }

  'Infix - {
    val input = s"""mX.funcCall(32, 32) """
    val Parsed.Success(s, _) = parse(input, parser.Exp(_))
    val matches = s match {
      case Def(FuncCall(Term("mX"), Term("funcCall"), Seq(Seq(a,b)))) => true // Correct
      case Def(d) =>
        graph.error(s.ctx, s"Input was parsed as $d")
        graph.error(s.ctx)
        false
    }
    require(matches, "Parser error.")
  }

}}
