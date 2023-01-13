
import tyes.runtime.*
import example.*
import scala.reflect.{ClassTag, TypeTest}

class TypeError(message: String) extends Exception(message)

object LambdaCalculusTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  def newCast
    [TargetT]
    (t: T, errorMessagePrefix: String = "not a ")
    (using tt: TypeTest[T, TargetT], ct: ClassTag[TargetT])
  : TargetT =
    t match {
      case t: TargetT => t
      case _ => throw new TypeError(s"$errorMessagePrefix${ct.runtimeClass.getSimpleName}")
    }
  
  enum Type extends tyes.runtime.Type:
    case Const
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  // To avoid changing the interface just for this test code, keep it and use the new
  // type in an auxiliary method
  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] =
    try Right(typecheckAux(exp, env))
    catch case exc => Left(s"TypeError: ${exc.getMessage}")

  def typecheckAux(exp: LExpression[Type], env: Map[String, Type]): Type = exp match {
    case LNumber(_c1) => 
      if _c1 == 1 then
        Type.Const
      else
        throw new TypeError(s"no type for `$exp`")

    case LVariable(x) =>
      if env.contains(x) then
        env(x)
      else
        throw new TypeError(s"no type for identifier `$x`")
  
    case LApp(e1, e2) =>
      val t1 = newCast[Type.$FunType](typecheckAux(e1, env), s"expected `$e1` to have type ")
      val t2 = typecheckAux(e2, env)

      if t2 == t1.t1 then
        t1.t2
      else
        throw new TypeError(s"types `$t2` and `${t1.t1}` don't match")

    case LFun(x, t, e) =>
      if t.isEmpty then
        throw new TypeError(s"no type provided in `${LFun(x, t, e)}`")
      
      val t1 = typecheckAux(e, env ++ Map(x -> t.get))
      Type.$FunType(t.get, t1)

    case _ => throw new TypeError(s"no type for `$exp`")
  }
