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
      for
        t <- typecheck(e1, env)
        t2 <- typecheck(e2, env)
        resT <- 
          (
            if t2 == t then
              Right(t)
            else if t2 == Type.Pos && t == Type.Zero then
              Right(Type.Pos)
            else
              TypeError.oneOf(TypeError.unexpectedType(t2, t), TypeError.allOf(TypeError.unexpectedType(t2, Type.Pos), TypeError.unexpectedType(t, Type.Zero)))
          ).orElse(
            if t2 == Type.Zero && t == Type.Pos then
              Right(Type.Pos)
            else
              TypeError.allOf(TypeError.unexpectedType(t2, Type.Zero), TypeError.unexpectedType(t, Type.Pos))
      )
      yield
        resT

    case _ => TypeError.noTypeFor(exp)
  }
