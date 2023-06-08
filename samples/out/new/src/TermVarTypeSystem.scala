import tyes.runtime.*
import example.*

class TermVarTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real
    case Sumpi

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
  }
