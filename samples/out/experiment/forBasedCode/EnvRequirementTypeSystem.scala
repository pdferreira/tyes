
import tyes.runtime.*
import example.*

extension [A,B](v: Either[A, B])
  def withFilter[C](f: B => Either[A, C]): Either[A, C] = v.flatMap(f)
  def and[C](other: => Either[A, C]): Either[A, C] = v.flatMap(_ => other)
  def not: Either[B, A] = v.swap
  def or(other: => Either[A, B]): Either[A, B] = 
    if v.isRight 
    then v
    else other.orElse(v)

object EnvRequirementTypeSystem extends TypeSystem[LExpression], TypeOperations:
  type T = Type

  enum Type extends tyes.runtime.Type:
    case Int
    case Real

  def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
    case LNumber(_c1) =>
      if _c1 == 1 then
        // Rule: PiTaker
        for
          _ <- Either.cond(env.size == 1, (), "TypeError: expected a single variable in scope")
          piT <- env.get("pi").toRight("TypeError: expected 'pi' to be in scope")
          if Either.cond(piT == Type.Real, (), s"TypeError: expected 'pi' to have type ${Type.Real}, but it has type $piT")
        yield
          Type.Real
      else if _c1 == 3 then
        // Rule: GenericPiTaker
        for
          _ <- Either.cond(env.size == 1, (), "TypeError: expected a single variable in scope")
          piT <- env.get("pi").toRight("TypeError: expected 'pi' to be in scope")
        yield
          piT
      else if _c1 == 4 then
        // Rule: NeutralReal
        Right(Type.Real)
      else if _c1 == 5 then
        // Rule: NeutralInt
        Right(Type.Int)
      else
        Left(s"TypeError: no type for 'LNumber($_c1)'")

      //// Or, factored

      if _c1 == 1 || _c1 == 3 then
        for
          _ <- Either.cond(env.size == 1, (), "TypeError: expected a single variable in scope")
          piT <- env.get("pi").toRight("TypeError: expected 'pi' to be in scope")
          res <-
            // Rule: PiTaker
            if _c1 == 1 then
              Either.cond(piT == Type.Real, Type.Real, s"TypeError: expected 'pi' to have type ${Type.Real}, but it has type $piT")
            
            // Rule: GenericPiTaker
            else if _c1 == 3 then
              Right(piT)

            else
              throw new Exception("Unexpected case, 1 and 3 already handled")
        yield
          res

      // Or

      // if _c1 == 1 then
      //   // Rule: PiTaker
      //   if env.size != 1 then
      //     Left(s"TypeError: expected a single variable in scope")
      //   else if !env.contains("pi") then
      //     Left(s"TypeError: ...")
      //   else if !env("pi") == Type.Real then
      //     Left(s"TypeError: ...")
      //   else
      //     Right(Type.Real)
      // else if _c1 == 3 then
      //   // Rule: GenericPiTaker
      //   if env.size != 1 then
      //     Left(s"TypeError: expected a single variable in scope")
      //   else if !env.contains("pi") then
      //     Left(s"TypeError: ...")
      //   else
      //     Right(env(pi))
      // else if _c1 == 4 then
      //   // Rule: NeutralReal
      //   Right(Type.Real)
      // else if _c1 == 5 then
      //   // Rule: NeutralInt
      //   Right(Type.Int)
      // else
      //   Left(s"TypeError: no type for '$exp'")

      ////// Alternative

      // if _c1 == 1 then
      //   // Rule: PiTaker
      //   if env.size != 1 then
      //     Left(s"TypeError: expected a single variable in scope")
      //   else
      //     val piT = env.get("pi")
      //     if !piT.isDefined then
      //        Left(s"TypeError: ...")
      //     else if piT.get != Type.Real then
      //       Left(s"TypeError: ...")
      //     else
      //       Right(Type.Real)

      ////// Alternative

      // if _c1 == 1 then
      //   // Rule: PiTaker
      //   if env.size != 1 then
      //     return Left(s"TypeError: expected a single variable in scope")
        
      //   val piT = env.get("pi")
      //   if !piT.isDefined then
      //     return Left(s"TypeError: ...")
        
      //   if piT.get != Type.Real then
      //     return Left(s"TypeError: ...")
        
      //   return Right(Type.Real)

      // if _c2 == 2 then
      //   ...

    case LPlus(e, _e2) =>
      if _e2 == LNumber(1) then
        typecheck(e, Map("pi" -> Type.Real))
      else if _e2 == LNumber(2) then
        typecheck(e, Map("pi" -> Type.Int))
      else if _e2 == LNumber(3) then
        for
          t3 <- typecheck(e, env)
          t4 <- typecheck(LNumber(1), Map("pi" -> t3))
          if Either.cond(t4 == t3, (), s"TypeError: types `$t4` and `$t3` don't match")
        yield
          t3
      else
        Left(s"TypeError: no type for `LPlus(_, $_e2)`")

    case _ => Left(s"TypeError: no type for `$exp`")
  }
