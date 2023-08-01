import tyes.runtime.*
import example.*

class TermVarTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real
    case Sumpi

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LVariable(v) => 
      if v == "pi" then
        for
          _ <- checkEnvSize(env, 1)
          t <- env.get("pi")
        yield
          t
      else if v == "const" then
        for
          _ <- checkEnvSize(env, 2)
          _ <- env.get("pi")
          t1 <- env.get("const")
        yield
          t1
      else
        TypeError.noTypeFor(exp)

    case LPlus(e1, e2) => 
      if e1 == LVariable("pi") && e2 == LNumber(1) then
        for
          _ <- typecheck(LVariable("pi"), Environment("pi" -> Type.Real)).expecting(Type.Real)
        yield
          Type.Sumpi
      else if e1 == LVariable("const") && e2 == LNumber(1) then
        for
          _ <- checkEnvSize(env, 1)
          t2 <- env.get("pi")
          _ <- typecheck(LVariable("const"), Environment("pi" -> t2, "const" -> Type.Int)).expecting(Type.Int)
        yield
          Type.Sumpi
      else
        TypeError.noTypeFor(exp)

    case LLet(x, _, e1, e2) => 
      for
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, Environment(x -> t1))
      yield
        t2

    case _ => TypeError.noTypeFor(exp)
  }
