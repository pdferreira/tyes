
import tyes.runtime.*
import example.*

object LambdaSimRecTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case Two
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
        Right(Type.One)
      else  if _c1 == 2 then
        Right(Type.Two)
      else  
        Left(s"TypeError: no type for `$exp`")
    
    case LVariable(f) =>
      if !env.contains(f) then
        return Left(s"TypeError: '$f' is not in scope")

      val Type.$FunType(t2, t) = cast[Type.$FunType](s"expected `$f` to have type ")(env(f)) match {
        case Right(ft) => ft
        case left => return left
      }
      
      Right(Type.$FunType(t, t2))

      // Alternative, not giving names to fun type args

      // val ft = cast[Type.$FunType](s"expected `$f` to have type ")(env(f)) match {
      //   case Right(ft) => ft
      //   case left => return left
      // }
      
      // Right(Type.$FunType(ft.t2, ft.t))

    case LPlus(e1, e2) => 
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

    case LLet(f, _ct2, e, _e4) => 
      if _e4 == LVariable("rec") then
        if _ct2.isEmpty then
          return Left(s"TypeError: no type provided in ${LLet(f, _ct2, e, _e4)}")
        
        val Type.$FunType(t, t2) = cast[Type.$FunType](s"expected `$f` to have type ")(_ct2.get) match {
          case Right(ft) => ft
          case left => return left
        }
        
        val t1 = typecheck(e, env ++ Map(f -> Type.$FunType(t2, t))).flatMap(newCast[Type.$FunType](s"expected `$e` to have type ")) match {
          case Right(t1) => t1
          case left => return left
        }

        if t != t1.t1 then
          return Left(s"TypeError: types `$t` and `${t1.t1}` don't match")
        else if t2 != t1.t2 then
          return Left(s"TypeError: types `$t2` and `${t1.t2}` don't match")
        
        Right(Type.$FunType(t, t2))
      else
        Left(s"TypeError: expected `LLet(_, _, _, $_e4)` to be one of: LVariable(\"rec\")")

    case _ => Left(s"TypeError: no type for `$exp`")
  }
