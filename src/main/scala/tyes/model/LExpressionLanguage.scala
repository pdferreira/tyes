package tyes.model

sealed trait LExpression
case class LNumber(num: Int) extends LExpression