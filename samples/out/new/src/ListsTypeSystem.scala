import tyes.runtime.*
import example.*

class ListsTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case List
    case One
    case Two

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNil => Right(Type.List)
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        TypeError.noTypeFor(exp)

    case LList(e1, e2) => 
      if e2.isInstanceOf[LList[?]] then
        for
          t <- typecheck(e1, env)
          LList(e3, r) = e2: @unchecked
          _ <- typecheck(e3, env).expecting(t)
          _ <- typecheck(r, env).expecting(Type.List)
        yield
          Type.List
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
