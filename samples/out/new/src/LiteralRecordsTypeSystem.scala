import tyes.runtime.*
import example.*

class LiteralRecordsTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case $EmptyRecord
    case Rec
    case Ref
    case $FunType(t1: Type, t2: Type)
    case $Record(l1: Label, t2: Type, t3: Type)

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LEmptyRecord => Right(Type.Rec)
    case LRecord(x, e1, e3) => 
      if e3.isInstanceOf[LRecord[?]] then
        for
          t1 <- typecheck(e1, env)
          LRecord(y, e2, ec) = e3: @unchecked
          t2 <- typecheck(e2, env)
          _ <- checkIf(ec == LEmptyRecord, TypeError.noTypeFor(ec))
        yield
          Type.$FunType(Type.$Record(x, t1, Type.$Record(y, t2, Type.$EmptyRecord)), Type.Rec)
      else if e3 == LEmptyRecord && x == "content" then
        for
          t <- typecheck(e1, env)
        yield
          Type.$FunType(t, Type.Ref)
      else
        TypeError.noTypeFor(exp)

    case _ => TypeError.noTypeFor(exp)
  }
