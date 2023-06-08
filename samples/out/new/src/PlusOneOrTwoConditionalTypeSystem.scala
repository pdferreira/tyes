import tyes.runtime.*
import example.*

class PlusOneOrTwoConditionalTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case PlusTwo
    case Two

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e1, e2) => 
      (
        for
          t1 <- typecheck(e1, env).expecting(Type.One)
          t2 <- typecheck(e2, env).expecting(Type.One)
        yield
          Type.One
      ).orElse(
        for
          t1 <- typecheck(e2, env).expecting(Type.Two)
        yield
          Type.PlusTwo
      )

    case _ => TypeError.noTypeFor(exp)
  }
