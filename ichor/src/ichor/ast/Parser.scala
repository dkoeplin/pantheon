package ichor.ast

import scala.util.parsing.combinator.Parsers
import scala.collection.mutable
import scala.util.parsing.input.Reader

class Parser extends Parsers {
  type Elem = Token

  private val lexer = new Lexer
  private val keywordCache = mutable.HashMap[String, Parser[String]]()

  /** A parser which matches a single keyword token.
    *
    * @param chars    The character string making up the matched keyword.
    * @return a `Parser` that matches the given string
    */
  implicit def keyword(chars : String): Parser[String] = {
    assert(lexer.delimiters.contains(chars) || lexer.keywords.contains(chars), s"Missing reserved string $chars")
    keywordCache.getOrElseUpdate(chars, accept(Keyword(chars)) ^^ (_.chars))
  }

  /** Matches a term. */
  val term: Parser[Term] = elem("term", _.isInstanceOf[Term]) ^^ (_.asInstanceOf[Term])

  /** Matches a prefix function name. */
  val prefixTerm: Parser[Term] = elem("prefix", {
    case Term(t) => t.length == 1 && lexer.prefixChars.contains(t.charAt(0))
    case _ => false
  }) ^^ (_.asInstanceOf[Term])

  /** Matches a type. */
  val typ:  Parser[Type] = elem("type", _.isInstanceOf[Type]) ^^ (_.asInstanceOf[Type])

  /** A parser which matches a numeric literal */
  val number: Parser[NumericLiteral] = elem("number", _.isInstanceOf[NumericLiteral]) ^^ (_.asInstanceOf[NumericLiteral])

  /** A parser which matches a string literal */
  val string: Parser[StringLiteral] = elem("string literal", _.isInstanceOf[StringLiteral]) ^^ (_.asInstanceOf[StringLiteral])

  /** A literal. */
  val literal: Parser[AST] = (
      number ^^ {t => Const(t) : AST }
    | string ^^ {t => Const(t) : AST }
  )

  val bind: Parser[Bind] = term ~ "@" ~ typ ^^ {case t ~ "@" ~ tp => Bind(t, tp) }

  val parameter: Parser[ValDef] = typ ~ term ~ ("=" ~> complexExpr).? ^^ {case tp ~ name ~ rhs => ValDef(tp,name,rhs) }

  val parameters: Parser[Seq[ValDef]] = repsep(parameter, ",") ^^ {params => params}

  val funcDef: Parser[FuncDef] = typ ~ term ~ ("(" ~> parameters <~ ")") ~ ("=" ~> complexExpr).? ^^ {
    case tp ~ name ~ params ~ rhs => FuncDef(tp, name, params, rhs)
  }

  /** Any series of tokens with a meaningful return value. */
  def expr: Parser[AST] = positioned(
      infix
    | dotInfix
    | assign
    | expr1
    | funcDef
    | valDef
  )

  def expr1: Parser[AST] = positioned(
      prefix
    | expr2
  )

  def expr2: Parser[AST] = positioned(
      tuple
    | bind
    | funcCall
    | scope
    | literal
  )

  /** A value or type declaration. */
  def complexExpr: Parser[AST] = expr

  val valDef: Parser[ValDef] = typ ~ term ~ ("=" ~> complexExpr).? ^^ {case tp ~ name ~ rhs => ValDef(tp, name, rhs) }

  val assign: Parser[AST] = term ~ "=" ~ complexExpr ^^ { case a ~ "=" ~ b => Assign(a, b) }

  /** Any expression inside parentheses. */
  val tuple: Parser[Tuple] = "(" ~> repsep(complexExpr, ",") <~ ")" ^^ {elems => Tuple(elems) }

  val args: Parser[Seq[AST]] = repsep(complexExpr, ",") ^^ {args => args}

  val funcCall: Parser[FuncCall] = term ~ tuple.? ^^ {case a ~ l => FuncCall(None, a, l) }

  val dotInfix: Parser[AST]
    = expr1 ~ ("." ~> funcCall).+ ^^ {case r ~ calls => calls.foldLeft(r){(r,call) => call.copy(recip = Some(r)) } }

  val statement: Parser[AST] = complexExpr

  val body: Parser[Scope] = rep(statement) ^^ {stms => Scope(bracketed = false, stms) }

  val switchcase: Parser[Case] = ("|" ~> complexExpr <~ ":") ~ body ^^ {case  cond ~ block => Case(cond, block) }

  val switch: Parser[Switch] = switchcase.+ ^^ {cases => Switch(cases) }

  val program: Parser[Scope] = phrase(body)

  /** Any series of expressions inside curly brackets. */
  val scope: Parser[AST] = "{" ~> (switch | body) <~ "}" ^^ {
    case b: Scope  => b.copy(bracketed = true)
    case s: Switch => s
    case _ => throw new Exception("Unreachable")
  }

  // (((a + b) + c) + d)
  def infix: Parser[AST] = expr1 ~ (term ~ expr1).+ ^^ {case r ~ calls =>
    calls.foldLeft(r){case (left, func ~ right) => FuncCall(Some(left), func, Some(Tuple(Seq(right)))) }
  }

  def prefix: Parser[AST] = prefixTerm ~ expr2 ^^ {case t ~ e => FuncCall(Some(e), Term("prefix_"+t), None) }

  def apply(file: String): Option[Scope] = {
    val scanner: Reader[Token] = lexer.scanner(file)

    var prev: (Int,Int) = (0,0)
    while (!scanner.atEnd) {
      try {
        program(scanner) match {
          case Success(prog, _)   => return Some(prog)
          case Failure(msg, next) if prev != (next.pos.line,next.pos.column) =>
            println(file + ":" + next.pos.line + ": " + msg)
            println(next.pos.longString)
            prev = (next.pos.line,next.pos.column)
          case Error(msg,next) if prev != (next.pos.line,next.pos.column) =>
            println(file + ":" + next.pos.line + ": " + msg)
            println(next.pos.longString)
            prev = (next.pos.line,next.pos.column)
          case _ => return None
        }
      }
      catch {
        case t: Throwable =>
          val pos = scanner.pos
          println(s"$file:${pos.line}:${pos.column}: Parser encountered error.")
          println(pos.longString)
          println(t)
          val trace = t.getStackTrace
          trace.take(10).foreach{line => println(line) }
          if (trace.length > 10) println("...")
          trace.takeRight(10).foreach{line => println(line) }

          var rest = scanner
          while (!rest.atEnd) {
            print(rest.first.toAST)
            rest = rest.rest
          }

          return None
      }
    }

    None
  }

}
