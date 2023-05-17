import tyes.runtime.*
import example.*

class LambdaCalculusTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Const
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case _ => Left(s"TypeError: no type for `${exp}`")
  }
