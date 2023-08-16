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
      for
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, env)
        resT <- 
          (
            if t2 == Type.Zero && t1 == Type.Zero then
              Right(Type.Zero)
            else if t2 == Type.Pos then
              Right(Type.Pos)
            else
              TypeError.oneOf(TypeError.allOf(TypeError.unexpectedType(t2, Type.Zero), TypeError.unexpectedType(t1, Type.Zero)), TypeError.unexpectedType(t2, Type.Pos))
          ).orElse(
            if t1 == Type.Pos then
              Right(Type.Pos)
            else
              TypeError.unexpectedType(t1, Type.Pos)
      )
      yield
        resT

    case _ => TypeError.noTypeFor(exp)
  }
