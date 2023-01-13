
import tyes.runtime.*
import example.*
import scala.reflect.{ClassTag, TypeTest}

class TypeError(message: String) extends Exception(message)

object LambdaSimRecTypeSystem extends TypeSystem[LExpression], TypeOperations:
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
    case One
    case Two
    case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)

  // To avoid changing the interface just for this test code, keep it and use the new
  // type in an auxiliary method
  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] =
    try Right(typecheckAux(exp, env))
    catch case exc => Left(s"TypeError: ${exc.getMessage}")

  def typecheckAux(exp: LExpression[Type], env: Map[String, Type]): Type = exp match {
    case LNumber(_c1) => 
      if _c1 == 1 then
        Type.One
      else  if _c1 == 2 then
        Type.Two
      else  
        throw new TypeError(s"no type for `$exp`")
    
    case LVariable(f) =>
      if !env.contains(f) then
        throw new TypeError(s"'$f' is not in scope")

      val Type.$FunType(t2, t) = newCast[Type.$FunType](env(f), s"expected `$f` to have type ")
      Type.$FunType(t, t2)

      // Alternative, not giving names to fun type args

      // val ft = newCast[Type.$FunType](env(f), s"expected `$f` to have type ")
      // Type.$FunType(ft.t2, ft.t)

    case LPlus(e1, e2) => 
      val t1 = newCast[Type.$FunType](typecheck(e1, env), s"expected `$e1` to have type ")
      val t2 = typecheck(e2, env)

      if t2 == t1.t1 then
        t1.t2
      else
        throw new TypeError(s"types `$t2` and `${t1.t1}` don't match")

    case LLet(f, _ct2, e, _e4) => 
      if _e4 == LVariable("rec") then
        if _ct2.isEmpty then
          throw new TypeError(s"no type provided in ${LLet(f, _ct2, e, _e4)}")
        
        val Type.$FunType(t, t2) = newCast[Type.$FunType](_ct2.get, s"expected `$f` to have type ")
        
        val t1 = newCast[Type.$FunType](typecheck(e, env ++ Map(f -> Type.$FunType(t2, t))), s"expected `$e` to have type ")

        if t != t1.t1 then
          throw new TypeError(s"types `$t` and `${t1.t1}` don't match")
        else if t2 != t1.t2 then
          throw new TypeError(s"types `$t2` and `${t1.t2}` don't match")
        
        Type.$FunType(t, t2)
      else
        throw new TypeError(s"expected `LLet(_, _, _, $_e4)` to be one of: LVariable(\"rec\")")

    case _ => throw new TypeError(s"no type for `$exp`")
  }
