import tyes.runtime.*
import example.*

class TermVarTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real
    case Sumpi

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LVariable(v) => ???
    case LPlus(e1, e2) => ???
    case LLet(x, _, e1, e2) => ???
    case _ => Left(s"TypeError: no type for `${exp}`")
  }
