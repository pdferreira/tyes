import tyes.runtime.*
import example.*

class PolymorphicSumV2TypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e_1, e_2) => 
      for
        t <- typecheck(e_1, env)
        _ <- typecheck(e_2, env).expecting(t)
      yield
        t

    case _ => TypeError.noTypeFor(exp)
  }
