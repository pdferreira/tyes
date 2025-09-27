import tyes.runtime.*
import example.*

class PolymorphicSumV2TypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        TypeError.noTypeFor(exp)

    case LPlusRange(es) => 
      for
        t <- typecheck(es(1), env)
        _ <- (1 until es.size).foldRange(Seq(t))(i => typecheck(es(i), env).expecting(t))
      yield
        t

    case _ => TypeError.noTypeFor(exp)
  }

  object LPlusRange:
    def unapply(exp: LExpression[Type]): Option[Seq[LExpression[Type]]] = exp.extractRange({
      case LPlus(l, r) => (l, r)
    })
