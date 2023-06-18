import tyes.runtime.*
import example.*

class LambdaSimRecTypeSystem extends TypeSystem[LExpression]:
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

    case LVariable(f) => 
      for
        _ft <- env.get(f).expecting[Type.$FunType]
        Type.$FunType(t2, t) = _ft
      yield
        Type.$FunType(t, t2)

    case LPlus(e1, e2) => 
      for
        _ft <- typecheck(e1, env).expecting[Type.$FunType]
        Type.$FunType(t, t2) = _ft
        t3 <- typecheck(e2, env)
        _ <- checkIf(t3 == t, TypeError.unexpectedType(t3, t))
      yield
        t2

    case LLet(f, _ft2, e, e4) => 
      if e4 == LVariable("rec") then
        for
          _ft <- checkTypeDeclared(_ft2, exp).expecting[Type.$FunType]
          Type.$FunType(t, t2) = _ft
          _ft3 <- typecheck(e, env + (f -> Type.$FunType(t2, t))).expecting[Type.$FunType]
          Type.$FunType(t3, t4) = _ft3
          _ <- checkIf(t3 == t, TypeError.unexpectedType(t3, t))
          _ <- checkIf(t4 == t2, TypeError.unexpectedType(t4, t2))
        yield
          Type.$FunType(t, t2)
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
