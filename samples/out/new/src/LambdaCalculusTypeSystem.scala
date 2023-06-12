import tyes.runtime.*
import example.*

class LambdaCalculusTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Const
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.Const)
      else
        TypeError.noTypeFor(exp)

    case LVariable(x) => env.get(x)
    case LApp(e1, e2) => 
      for
        t1 <- typecheck(e1, env).expecting({
          case ft: Type.$FunType => ft
        })
        t2 <- typecheck(e2, env)
        _ <- checkIf(t1.t1 == t2, TypeError.unexpectedType(t2, t1.t1))
      yield
        t1.t2

    case LFun(x, _t, e) => 
      for
        t <- checkTypeDeclared(_t, exp)
        t2 <- typecheck(e, env + (x -> t))
      yield
        Type.$FunType(t, t2)

    case _ => TypeError.noTypeFor(exp)
  }
