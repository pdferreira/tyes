import tyes.runtime.*
import example.*

class AnyTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Any

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case e => Right(Type.Any)
    case _ => Left(s"TypeError: no type for `${exp}`")
  }
