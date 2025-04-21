import tyes.runtime.*
import example.*

class RegularCasesV2TypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e_1, e_2) => 
      for
        _ <- typecheck(e_1, env).expecting(Type.One)
        _ <- typecheck(e_2, env).expecting(Type.One)
      yield
        Type.One

    case _ => TypeError.noTypeFor(exp)
  }
