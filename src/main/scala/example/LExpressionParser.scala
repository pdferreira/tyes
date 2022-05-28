package example

import scala.util.parsing.combinator.RegexParsers

object LExpressionParser extends RegexParsers:

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r

  def variable = ident ^^ { varName => LVariable(varName) } 

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => LNumber(numStr.toInt) }
  
  def leaf = number | variable

  def operator = leaf ~ ("+" ~> leaf).* ^^ { 
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => LPlus(left, right) }
  }
  
  def expression: Parser[LExpression] = operator

  def parse(input: String) = super.parse(phrase(expression), input)
