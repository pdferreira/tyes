import tyes.runtime.*
import example.*

class PosOrZeroTypeSystem extends TypeSystem[LExpression]:
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
            t <- typecheck(e1, env)
            _ <- typecheck(e2, env).expecting(t)
          yield
            t
        ).orElse(
          for
            _ <- typecheck(e1, env).expecting(Type.Zero)
            _ <- typecheck(e2, env).expecting(Type.Pos)
          yield
            Type.Pos
        )
      ).orElse(
        for
          _ <- typecheck(e1, env).expecting(Type.Pos)
          _ <- typecheck(e2, env).expecting(Type.Zero)
        yield
          Type.Pos
      )

    case _ => TypeError.noTypeFor(exp)
  }
