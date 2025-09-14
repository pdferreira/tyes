import tyes.runtime.*
import example.*

class SequenceTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two
    case Zero
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNil => Right(Type.Zero)
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

    case LList(e_1, e2) => 
      if e2.isInstanceOf[LList[?]] then
        for
          t_1 <- typecheck(e_1, env)
          LList(e_2, eb) = e2: @unchecked
          t_2 <- typecheck(e_2, env)
          _ <- eb.expecting[LList[Type]]
          LList(e_3, l) = eb: @unchecked
          t_3 <- typecheck(e_3, env)
          t_l <- typecheck(l, env)
        yield
          Type.$FunType(t_1, Type.$FunType(t_2, Type.$FunType(t_3, t_l)))
      else
        TypeError.noTypeFor(exp)

    case LLet(x_1, _, e_0, e4) => 
      if e4.isInstanceOf[LLet[?]] then
        for
          t_0 <- typecheck(e_0, env)
          LLet(x_2, _, e_1, ed) = e4: @unchecked
          _ <- typecheck(e_1, Environment(x_1 -> t_0))
          _ <- ed.expecting[LLet[Type]]
          LLet(x_3, _, e_2, e_3) = ed: @unchecked
          _ <- typecheck(e_2, Environment(x_2 -> t_0))
          t_3 <- typecheck(e_3, Environment(x_3 -> t_0))
        yield
          t_3
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
