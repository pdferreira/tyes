import tyes.runtime.*
import example.*

class LambdaSimTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

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
        _ft <- typecheck(e1, env).expecting[Type.$FunType]
        Type.$FunType(t, t2) = _ft: @unchecked
        _ <- typecheck(e2, env).expecting(t)
      yield
        t2

    case LLet(x, _t, e, e4) => 
      if e4 == LVariable("noop") then
        for
          t <- checkTypeDeclared(_t, exp)
          t2 <- typecheck(e, Environment(x -> t))
        yield
          Type.$FunType(t, t2)
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
