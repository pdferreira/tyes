import tyes.runtime.*
import example.*

class RangesTypeSystem extends TypeSystem[LExpression]:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case $EmptyRecord
    case One
    case Two
    case $FunType(t1: Type, t2: Type)
    case $Record(l1: Label, t2: Type, t3: Type)

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

    case LPlusRange((es)) => 
      for
        _ftr <- typecheck(es.tail.foldLeft(es.head)(LApp.apply), env).expecting[Type.$FunType]
        $FunTypeRange((ts)) = _ftr: @unchecked
      yield
        ts.last

    case LLet(x, _r2, e_a, e_b) => 
      for
        _rr <- checkTypeDeclared(_r2, exp).expecting[Type.$Record]
        $RecordRange((_, ts, tc)) = _rr: @unchecked
        _ <- typecheck(e_a, env).expecting(ts(0))
        _ <- typecheck(e_b, env).expecting(ts.last)
        _ <- tc.expecting(Type.$EmptyRecord)
      yield
        ts.init.foldRight(ts.last)(Type.$FunType.apply)

    case _ => TypeError.noTypeFor(exp)
  }

  object LPlusRange:
    def unapply(exp: LExpression[Type]) = exp.extractRangeLNoSeed[LPlus[Type]]

  object $FunTypeRange:
    def unapply(typ: Type) = typ.extractRangeRNoSeed[Type.$FunType]

  object $RecordRange:
    def unapply(typ: Type) = typ.extractRangeR[Type.$Record]
