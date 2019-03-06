package eos.test.ast

import fastparse._
import utils.implicits.terminal._

import scala.util.control.NoStackTrace

trait ASTTests {

  import eos.ast.implicits.{ASTEquivalent,ast}

  def parse_check[T](parser: P[_] => P[T])(input: String, expect: T): T = {
    parse(input, parser) match {
      case Parsed.Success(result,_) =>
        val matches = ASTEquivalent(result, expect)
        if (!matches) {
          Console.out.error(s"Expect: ${ast(expect)}")
          Console.out.error(s"Result: ${ast(result)}")
          throw new Exception("Parsing failed.") with NoStackTrace
        }
        result

      case Parsed.Failure(_,_,_) =>
        val out = parse(input, parser, verboseFailures = true)
        val Parsed.Failure(label,idx,extra) = out
        Console.out.error("Parsing error:")
        Console.out.error("label: " + label)
        Console.out.error("idx: " + idx)
        Console.out.error("extra: " + extra)
        throw new Exception("Parsing failed.") with NoStackTrace
    }
  }

}
