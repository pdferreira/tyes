import tyes.runtime.*
import example.*

class RecursiveRecordsTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Rec

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LEmptyRecord => Right(Type.Rec)
    case LRecord(l, e, r) => 
      for
        _ <- typecheck(e, env)
        _ <- typecheck(r, env).expecting(Type.Rec)
      yield
        Type.Rec

    case _ => TypeError.noTypeFor(exp)
  }
