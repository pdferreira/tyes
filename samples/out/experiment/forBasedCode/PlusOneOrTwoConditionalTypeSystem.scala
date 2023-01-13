
import tyes.runtime.*
import example.*

object PlusOneOrTwoConditionalTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case One
    case PlusTwo
    case Two

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(_c1) => 
      if _c1 == 1 then
        Right(Type.One)
      else  if _c1 == 2 then
        Right(Type.Two)
      else  
        Left(s"TypeError: no type for `$exp`")

    case LPlus(e1, e2) => 
      (
        for
          t1 <- typecheck(e1, env)
          t2 <- typecheck(e2, env)
          if Either.cond(t1 == Type.One, (), s"Expected $t1 to be One") and
             Either.cond(t2 == Type.One, (), s"Expected $t2 to be One")
        yield
          Type.One
      ) or (
        for
          t3 <- typecheck(e2, env)
          if Either.cond(t3 == Type.Two, (), s"Expected $t2 to be Two")
        yield
          Type.PlusTwo
      )

      //// Alternative, factoring common and improving error message

      for
        t1 <- typecheck(e1, env)
        t2 <- typecheck(e2, env)
        res <- (
          if t2 == Type.One then
            Either.cond(t1 == Type.One, Type.One, s"Expected $t1 to be One")
          else if t2 == Type.Two then
            Right(Type.PlusTwo)
          else
            Left(s"Expected $t2 to be one of: One, Two")
        )
      yield
        res

    case _ => Left(s"TypeError: no type for `$exp`")
  }
