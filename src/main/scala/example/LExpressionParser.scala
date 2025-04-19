package example

import scala.util.parsing.combinator.RegexParsers

class LExpressionParser[TType] extends RegexParsers:

  val keywords = Set("let", "in", "fun")

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r.filter(id => !keywords.contains(id))

  def variable = ident ^^ { varName => LVariable(varName) } 

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => LNumber(numStr.toInt) }
  
  def list = "[" ~> repsep(expression, ",") <~ "]" ^^ {
    elems => elems.foldRight(LNil: LExpression[TType]) { (e, l) => LList(e, l) }
  }

  def leaf = ("(" ~> expression <~ ")") | number | variable | list

  def app = leaf ~ leaf.* ^^ {
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => LApp(left, right) }
  }

  def operator = app ~ ("+" ~> app).* ^^ { 
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => LPlus(left, right) }
  }

  def tpe: Parser[TType] = failure("No parser for types defined")

  def let: Parser[LExpression[TType]] = ("let" ~> ident) ~ (":" ~> tpe).? ~ ("=" ~> (expression - let)) ~ ("in" ~> expression) ^^ {
    case varName ~ varTypeOpt ~ varExp ~ inExp => LLet(varName, varTypeOpt, varExp, inExp)
  }

  def fun: Parser[LExpression[TType]] = ("fun" ~> ident) ~ (":" ~> tpe).? ~ ("=>" ~> expression) ^^ {
    case argName ~ argTypeOpt ~ bodyExp => LFun(argName, argTypeOpt, bodyExp)
  }

  def expression: Parser[LExpression[TType]] = let | fun | operator

  def parse(input: String) = super.parse(phrase(expression), input)
