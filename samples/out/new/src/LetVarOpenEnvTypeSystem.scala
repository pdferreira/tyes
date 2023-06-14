import tyes.runtime.*
import example.*

class LetVarOpenEnvTypeSystem extends TypeSystem[LExpression]:
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

    case LVariable(x) => env.get(x)
    case LPlus(e1, e2) => 
      for
        // need to have different cursors and they should be compared
        // maybe a `_ <- (...).expecting(t)`?
        t <- typecheck(e1, env)
        t <- typecheck(e2, env)
      yield
        t

    case LLet(x, _, e1, e2) => 
      for
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, env + (x -> t1))
      yield
        t2

    case _ => TypeError.noTypeFor(exp)
  }
