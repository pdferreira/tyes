import tyes.runtime.*
import example.*

class MetaVarTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(n) => ???
    case LVariable(v) => ???
    case LPlus(e1, e2) => ???
    case _ => Left(s"TypeError: no type for `${exp}`")
  }
