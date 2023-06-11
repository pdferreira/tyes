import tyes.runtime.*
import example.*

class MetaVarTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 0 then
        Right(Type.Int)
      else
        TypeError.noTypeFor(exp)

    case LVariable(v) => 
      if v == "pi" then
        Right(Type.Real)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e1, e2) => 

    case _ => TypeError.noTypeFor(exp)
  }
