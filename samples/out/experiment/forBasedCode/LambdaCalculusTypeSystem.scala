
import tyes.runtime.*
import example.*

extension [A,B](v: Either[A, B])
  def withFilter[C](f: B => Either[A, C]): Either[A, C] = v.flatMap(f)
  def and[C](other: Either[A, C]): Either[A, C] = v.flatMap(_ => other)

object LambdaCalculusTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Const
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

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
        Right(Type.Const)
      else
        Left(s"TypeError: no type for `$exp`")

    case LVariable(x) => 
      Either.cond(env.contains(x), env(x), s"TypeError: no type for identifier `$x`")
  
    case LApp(e1, e2) => 
      for
        t1 <- typecheck(e1, env).flatMap(newCast[Type.$FunType](s"expected `$e1` to have type "))
        t2 <- typecheck(e2, env)
        if Either.cond(t2 == t1.t1, (), s"TypeError: types `$t2` and `${t1.t1}` don't match")
      yield
        t1.t2

      ////// Alternative
      // _t1 <- typecheck(e1, env)
      // t1 <- cast[Type.$FunType](s"expected ...")
      // ...

      ////// Alternative
      // (
      // t1 <- ...
      // t2 <- ...
      // yield if t2 == t1.1 then Right(t1.t2) else Left("TypeError: ...")
      // ).flatten

    case LFun(x, t, e) => 
      for
        _t <- t.toRight(s"TypeError: no type provided in `${LFun(x, t, e)}`")
        t1 <- typecheck(e, env ++ Map(x -> _t))
      yield
        Type.$FunType(_t, t1)

    case _ => Left(s"TypeError: no type for `$exp`")
  }
