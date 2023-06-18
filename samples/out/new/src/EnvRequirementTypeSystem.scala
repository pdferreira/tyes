import tyes.runtime.*
import example.*

class EnvRequirementTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        for
          _ <- checkEnvSize(env, 1)
          _ <- env.get("pi").expecting(Type.Real)
        yield
          Type.Real
      else if n == 3 then
        for
          _ <- checkEnvSize(env, 1)
          t <- env.get("pi")
        yield
          t
      else if n == 4 then
        Right(Type.Real)
      else if n == 5 then
        Right(Type.Int)
      else
        TypeError.noTypeFor(exp)

    case LPlus(e, e2) => 
      if e2 == LNumber(1) then
        typecheck(e, Environment("pi" -> Type.Real))
      else if e2 == LNumber(2) then
        typecheck(e, Environment("pi" -> Type.Int))
      else if e2 == LNumber(3) then
        for
          t <- typecheck(e, env)
          t2 <- typecheck(LNumber(1), Environment("pi" -> t))
          // Missing check that t == t2
        yield
          t
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
