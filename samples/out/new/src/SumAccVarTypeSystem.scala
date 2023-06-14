import tyes.runtime.*
import example.*

class SumAccVarTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Three
    case Two

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else if n == 3 then
        Right(Type.Three)
      else
        TypeError.noTypeFor(exp)

    case LVariable(x) => 
      for
        _ <- checkEnvSize(env, 1)
        t <- env.get(x)
      yield
        t

    case LPlus(e1, e2) => 
      for
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, Environment("acc" -> t1))
      yield
        t2

    case _ => TypeError.noTypeFor(exp)
  }
