import tyes.runtime.*
import example.*

class PlusOneOrTwoConditionalTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case PlusTwo
    case Two

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(n) => ???
    case LPlus(e1, e2) => ???
    case _ => Left(s"TypeError: no type for `${exp}`")
  }
