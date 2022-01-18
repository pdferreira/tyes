package tyes.model

object LExpressionParser:
  import Parsers.*

  def number = ("0" | raw"[1-9]\d*".r).named("number") ^^ { numStr => LNumber(numStr.toInt) }
  def expression: Parser[LExpression] = number
  
  def toTermParser(using conv: Conversion[LExpression, Term]): Parser[Term] = expression ^^ conv.convert
  def parse(input: String) = Parsers.parse(phrase(expression), input)