package example

sealed trait LExpression
case class LNumber(num: Int) extends LExpression
case class LVariable(name: String) extends LExpression
case class LPlus(left: LExpression, right: LExpression) extends LExpression
case class LLet(varName: String, varExp: LExpression, inExp: LExpression) extends LExpression
