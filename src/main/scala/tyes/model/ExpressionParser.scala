package tyes.model

trait ExpressionParser[E]:
  def expression: Parsers.Parser[E]