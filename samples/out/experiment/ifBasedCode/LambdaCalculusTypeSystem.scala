
import tyes.runtime.*
import example.*

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
      if env.contains(x) then
        Right(env(x))
      else
        Left(s"TypeError: no type for identifier `$x`")
  
    case LApp(e1, e2) => 
      val t1 = typecheck(e1, env).flatMap(newCast[Type.$FunType](s"expected `$e1` to have type ")) match {
        case Right(t1) => t1
        case left => return left 
      }
      val t2 = typecheck(e2, env) match {
        case Right(t2) => t2
        case left => return left
      }

      if t2 == t1.t1 then
        Right(t1.t2)
      else
        Left(s"TypeError: types `$t2` and `${t1.t1}` don't match")

    case LFun(x, t, e) =>
      if t.isEmpty then
        Left(s"TypeError: no type provided in `${LFun(x, t, e)}`")
      else
        val t1 = typecheck(e, env ++ Map(x -> t.get)) match {
          case Right(t1) => t1
          case left => return left
        }
        
        Right(Type.$FunType(t.get, t1))

    case _ => Left(s"TypeError: no type for `$exp`")
  }
