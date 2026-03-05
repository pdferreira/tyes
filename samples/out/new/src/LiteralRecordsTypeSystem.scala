import tyes.runtime.*
import example.*

class LiteralRecordsTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Rec
    case Ref
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LEmptyRecord => Right(Type.Rec)
    case LRecord(x, e1, e3) => 
      if e3.isInstanceOf[LRecord[?]] then
        for
          t1 <- typecheck(e1, env)
          LRecord(_, e2, ec) = e3: @unchecked
          t2 <- typecheck(e2, env)
          _ <- checkIf(ec == LEmptyRecord, TypeError.noTypeFor(ec))
        yield
          Type.$FunType(t1, Type.$FunType(t2, Type.Rec))
      else if e3 == LEmptyRecord && x == "content" then
        for
          t <- typecheck(e1, env)
        yield
          Type.$FunType(t, Type.Ref)
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
