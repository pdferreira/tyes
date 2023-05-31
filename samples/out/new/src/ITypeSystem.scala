import tyes.runtime.*
import example.*

class ITypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.Int)
      else
        Left(s"TypeError: no type for ${exp}")

    case _ => Left(s"TypeError: no type for `${exp}`")
  }
