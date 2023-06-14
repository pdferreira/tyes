import tyes.runtime.*
import example.*

class PlusOneTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case SumOne

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e, e2) => 
      if e2 == LNumber(1) then
        Right(Type.SumOne)
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
