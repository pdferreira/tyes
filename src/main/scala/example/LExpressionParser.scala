package example

import scala.util.parsing.combinator.RegexParsers

object LExpressionParser extends RegexParsers:

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r

  def variable = ident ^^ { varName => LVariable(varName) } 

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => LNumber(numStr.toInt) }
  
  def leaf = number | variable

  def operator = leaf ~ ("+" ~> expression).? ^^ { 
    case exp ~ None => exp
    case left ~ Some(right) => LPlus(left, right)
  }
  
  def expression: Parser[LExpression] = operator

  def parse(input: String) = super.parse(phrase(expression), input)
