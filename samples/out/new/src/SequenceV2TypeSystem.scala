import tyes.runtime.*
import example.*

class SequenceV2TypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case List
    case One
    case Two
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  def typecheck(exp: LExpression[Type], env: Environment[Type]): Either[String, Type] = exp match {
    case LNil => Right(Type.List)
    case LNumber(n) => 
      if n == 1 then
        Right(Type.One)
      else if n == 2 then
        Right(Type.Two)
      else
        TypeError.noTypeFor(exp)

    case LListRange((es, e2)) => 
      if e2 == LNil then
        for
          t <- typecheck(es(0), env)
          _ <- (1 until es.size).foldRange(Seq(t))(i => typecheck(es(i), env).expecting(t))
        yield
          Type.List
      else
        TypeError.noTypeFor(exp)

    case LLetRange((x, _ts, es, b)) => 
      for
        t2 <- typecheck(b, env)
        ts <- (0 until _ts.size).foldRange(Seq())(i => checkTypeDeclared(_ts(i), exp))
        _ <- (0 until es.size).foldRange(Seq())(i => typecheck(es(i), env).expecting(ts(i)))
      yield
        Type.$FunType(ts(0), t2)

    case _ => TypeError.noTypeFor(exp)
  }

  object LListRange:
    def unapply(exp: LExpression[Type]) = exp.extractRangeR[LList[Type]]

  object LLetRange:
    def unapply(exp: LExpression[Type]) = exp.extractRangeR[LLet[Type]]
