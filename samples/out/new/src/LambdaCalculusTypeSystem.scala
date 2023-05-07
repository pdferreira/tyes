import tyes.runtime.*
import example.*

class LambdaCalculusTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case _ => Left(s"TypeError: no type for `${exp}`")
  }
