import tyes.runtime.*
import example.*

class SequenceV2TypeSystem extends TypeSystem[LExpression]:
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

    case LListRange(es, LNil) => 
      for
        t <- typecheck(es(0), env)
        _ <- (1 until es.size).foldRange(Seq(t))(i => typecheck(es(i), env).expecting(t))
      yield
        Type.$FunType(t, t)

    case _ => TypeError.noTypeFor(exp)
  }

  object LListRange:
    def unapply(exp: LExpression[Type]): Option[(Seq[LExpression[Type]], LExpression[Type])] = exp.extractRange(1, {
      case LList(l, r) => Seq(l, r)
    })
