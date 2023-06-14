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
      // missing destructure of f into t -> t2
      // missing check that env contains f with type t2 -> t 
      Right(Type.$FunType(t, t2))

    case LPlus(e1, e2) => 
      for
        t1 <- typecheck(e1, env).expecting[Type.$FunType]
        t <- typecheck(e2, env)
        _ <- checkIf(t1.t1 == t, TypeError.unexpectedType(t, t1.t1))
      yield
        t1.t2

    case LLet(f, _ct2, e, e4) => 
      if e4 == LVariable("rec") then
        for
          ct2 <- checkTypeDeclared(_ct2, exp).expecting[Type.$FunType]
          // missing destructure of ct2 into t -> t2 (or rather alias ct2.t1 and ct2.t2)
          // could be a simple Type.$FunType(t, t2) = ct2, since neither is declared
          t1 <- typecheck(e, env + (f -> Type.$FunType(t2, t))).expecting[Type.$FunType]
          // missing check that t1.t1 == t and t1.t2 == t2
        yield
          // if I go with the direct destructure instead of aliasing, here it would be
          // better to show up as `t` and `t2` simply
          Type.$FunType(t1.t1, t1.t2)
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
