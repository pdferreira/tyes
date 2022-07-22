package example

sealed trait LExpression[+TType]
case class LNumber(num: Int) extends LExpression[Nothing]
case class LVariable(name: String) extends LExpression[Nothing]
case class LPlus[TType](left: LExpression[TType], right: LExpression[TType]) extends LExpression[TType]
case class LLet[TType](varName: String, varType: Option[TType], varExp: LExpression[TType], inExp: LExpression[TType]) extends LExpression[TType]
