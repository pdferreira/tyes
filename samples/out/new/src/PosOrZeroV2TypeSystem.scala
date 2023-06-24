import tyes.runtime.*
import example.*

class PosOrZeroV2TypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Pos
    case Zero

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 0 then
        Right(Type.Zero)
      else if n == 1 then
        Right(Type.Pos)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e1, e2) => 
      (
        (
          for
            _ <- typecheck(e1, env).expecting(Type.Zero)
            _ <- typecheck(e2, env).expecting(Type.Zero)
          yield
            Type.Zero
        ).orElse(
          for
            t <- typecheck(e1, env)
            _ <- typecheck(e2, env).expecting(Type.Pos)
          yield
            Type.Pos
        )
      ).orElse(
        for
          _ <- typecheck(e1, env).expecting(Type.Pos)
          t <- typecheck(e2, env)
        yield
          Type.Pos
      )

    case _ => TypeError.noTypeFor(exp)
  }
