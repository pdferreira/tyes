import tyes.runtime.*
import example.*

class RangesTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        TypeError.noTypeFor(exp)

    case LApp(e1, e2) => 
      for
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, env)
      yield
        Type.$FunType(t1, t2)

    case LPlusRange(es) => typecheck(es.tail.foldLeft(es.head)(LApp.apply), env)
    case _ => TypeError.noTypeFor(exp)
  }

  object LPlusRange:
    def unapply(exp: LExpression[Type]): Option[Seq[LExpression[Type]]] = extractRange(exp, {
      case LPlus(l, r) => Tuple2(l, r)
    })
