import tyes.runtime.*
import example.*

class PlusOneOrTwoConditionalTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case PlusTwo
    case Two

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        Left(s"TypeError: no type for ${exp}")

    case LPlus(e1, e2) => 
      (
        for
          t1 <- typecheck(e1, env)
          t2 <- typecheck(e2, env)
          _ <- Either.cond(t1 == Type.One, (), s"TypeError: types ${Type.One} and ${t1} don't match")
          _ <- Either.cond(t2 == Type.One, (), s"TypeError: types ${Type.One} and ${t2} don't match")
        yield
          Type.One
      ).orElse(
        for
          t1 <- typecheck(e2, env)
          _ <- Either.cond(t1 == Type.Two, (), s"TypeError: types ${Type.Two} and ${t1} don't match")
        yield
          Type.PlusTwo
      )

    case _ => Left(s"TypeError: no type for `${exp}`")
  }
