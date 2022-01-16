package tyes.model

object LExpressionParser extends ExpressionParser[LExpression]:
  import Parsers.*

  def number = ("0" | raw"[1-9]\d*".r).named("number") ^^ { numStr => LNumber(numStr.toInt) }
  def expression: Parser[LExpression] = number
  def parse(input: String) = Parsers.parse(phrase(expression), input)