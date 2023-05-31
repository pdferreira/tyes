import tyes.runtime.*
import example.*

class OneOrTwoTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        Left(s"TypeError: no type for ${exp}")

    case _ => Left(s"TypeError: no type for `${exp}`")
  }
