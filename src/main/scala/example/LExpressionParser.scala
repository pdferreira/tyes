package example

import scala.util.parsing.combinator.RegexParsers

object LExpressionParser extends RegexParsers:

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r

  def variable = ident ^^ { varName => LVariable(varName) } 

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => LNumber(numStr.toInt) }
  
  def leaf = ("(" ~> expression <~ ")") | number | variable 

  def operator = leaf ~ ("+" ~> leaf).* ^^ { 
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => LPlus(left, right) }
  }

  def let = ("let" ~> ident) ~ ("=" ~> operator) ~ ("in" ~> expression) ^^ {
    case varName ~ varExp ~ inExp => LLet(varName, varExp, inExp)
  }
  
  def expression: Parser[LExpression] = let | operator

  def parse(input: String) = super.parse(phrase(expression), input)
