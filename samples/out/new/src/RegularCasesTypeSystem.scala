import tyes.runtime.*
import example.*

class RegularCasesTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else
        Left(s"TypeError: no type for ${exp}")

    case LPlus(e1, e2) => 
      for
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, env)
        _ <- Either.cond(t1 == Type.One, (), s"TypeError: types ${Type.One} and ${t1} don't match")
        _ <- Either.cond(t2 == Type.One, (), s"TypeError: types ${Type.One} and ${t2} don't match")
      yield
        Type.One

    case _ => Left(s"TypeError: no type for `${exp}`")
  }
