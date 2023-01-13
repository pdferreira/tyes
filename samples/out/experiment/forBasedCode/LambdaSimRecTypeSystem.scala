
import tyes.runtime.*
import example.*

extension [A, B](v: Either[A, B])
  def withFilter(f: B => Either[A, Unit]): Either[A, Unit] = v.flatMap(f)
  def and[C](other: => Either[A, C]): Either[A, C] = v.flatMap(_ => other)
  def not: Either[B, A] = v.swap
  def or(other: => Either[A, B]): Either[A, B] = 
    if v.isRight 
    then v
    else other.orElse(v)
    
object LambdaSimRecTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  import scala.reflect.{ClassTag, TypeTest}

  def newDestructure
    [TargetT <: CompositeType[T]]
    (typOpt: Option[T], errorIfMissing: => String = "no type provided", errorIfWrong: => String = null)
    (using tt: TypeTest[T, TargetT], ct: ClassTag[TargetT])
  : Either[String, Seq[T]] =
    typOpt match {
      case Some(typ) =>
        newCast[TargetT](errorIfWrong)(typ).map(rt => rt.innerTypes)
      case None =>
        Left(s"TypeError: $errorIfMissing")
    }

  def newCast
    [TargetT]
    (errorMessagePrefix: String = "not a ")
    (t: T)
    (using tt: TypeTest[T, TargetT], ct: ClassTag[TargetT])
  : Either[String, TargetT] =
    t match {
      case t: TargetT => Right(t)
      case _ => Left(s"TypeError: $errorMessagePrefix${getCompositeTypeName(ct.runtimeClass)}")
    }

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(_c1) => 
      if _c1 == 1 then
        Right(Type.One)
      else  if _c1 == 2 then
        Right(Type.Two)
      else  
        Left(s"TypeError: no type for `$exp`")
    
    case LVariable(f) => 
      for
        _df <- newDestructure[Type.$FunType](env.get(f), s"'$f' is not in scope")
        Seq(t2, t) = _df
      yield
        Type.$FunType(t, t2)

    case LPlus(e1, e2) => 
      for
        t1 <- typecheck(e1, env).flatMap(newCast[Type.$FunType](s"expected `$e1` to have type "))
        t2 <- typecheck(e2, env)
        if Either.cond(t2 == t1.t1, (), s"TypeError: types `$t2` and `${t1.t1}` don't match")
      yield
        t1.t2

    case LLet(f, _ct2, e, _e4) => 
      if _e4 == LVariable("rec") then
        for
          _dct2 <- newDestructure[Type.$FunType](_ct2, errorIfWrong = s"expected type `${_ct2.get}` to be a ")
          Seq(t, t2) = _dct2
          t1 <- typecheck(e, env ++ Map(f -> Type.$FunType(t2, t))).flatMap(newCast[Type.$FunType](s"expected `$e` to have type "))
          if Either.cond(t == t1.t1, (), s"TypeError: types `$t` and `${t1.t1}` don't match") and
             Either.cond(t2 == t1.t2, (), s"TypeError: types `$t2` and `${t1.t2}` don't match")
        yield
          Type.$FunType(t, t2)
      else
        Left(s"TypeError: expected `LLet(_, _, _, $_e4)` to be one of: LVariable(\"rec\")")

    case _ => Left(s"TypeError: no type for `$exp`")
  }
