import tyes.runtime.*
import example.*

class LetVarTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
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
        // need different cursors and comparison
        t <- typecheck(e1, env)
        t <- typecheck(e2, env)
      yield
        t

    case LLet(x, _t1, e1, e2) => 
      for
        t1 <- checkTypeDeclared(_t1, exp)
        // need different cursors and comparison
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, Environment(x -> t1))
      yield
        t2

    case _ => TypeError.noTypeFor(exp)
  }
