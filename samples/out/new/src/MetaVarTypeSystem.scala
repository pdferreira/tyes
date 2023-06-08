import tyes.runtime.*
import example.*

class MetaVarTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
  }
