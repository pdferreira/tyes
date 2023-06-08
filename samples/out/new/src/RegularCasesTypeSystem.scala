import tyes.runtime.*
import example.*

class RegularCasesTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e1, e2) => 
      for
        t1 <- typecheck(e1, env).expecting(Type.One)
        t2 <- typecheck(e2, env).expecting(Type.One)
      yield
        Type.One

    case _ => TypeError.noTypeFor(exp)
  }
