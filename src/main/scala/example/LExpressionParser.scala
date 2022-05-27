package example

import scala.util.parsing.combinator.RegexParsers

object LExpressionParser extends RegexParsers:

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => LNumber(numStr.toInt) }

  def operator = number ~ ("+" ~ expression).? ^^ { 
    case exp ~ None => exp
    case left ~ Some(_ ~ right) => LPlus(left, right) 
  }
  
  def expression: Parser[LExpression] = operator

  def parse(input: String) = super.parse(phrase(expression), input)
