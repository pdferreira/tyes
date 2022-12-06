
    import tyes.runtime.*
    import example.*
    
    object TermVarTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case Int
        case Real
        case Sumpi
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LVariable(_c1) => 
          if _c1 == "pi" && env.size == 1 && env.contains("pi") then
            Right(env("pi"))
          else  if _c1 == "const" && env.size == 2 && env.contains("pi") && env.contains("const") then
            Right(env("const"))
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(_e1, _e2) => 
          val _t1 = typecheck(LVariable("pi"), Map("pi" -> Type.Real))
          val _t2 = env.get("pi").toRight(s"'${"pi"}' not found").flatMap(t2 => typecheck(LVariable("const"), Map("pi" -> t2, "const" -> Type.Int)))
          if _e2 == LNumber(1) && _e1 == LVariable("pi") then
            if _t1 == Right(Type.Real) then
              Right(Type.Sumpi)
            else
              Left(s"TypeError: no type for `$exp`")
          else  if _e2 == LNumber(1) && _e1 == LVariable("const") && env.size == 1 && env.contains("pi") then
            if _t2 == Right(Type.Int) then
              Right(Type.Sumpi)
            else
              Left(s"TypeError: no type for `$exp`")
          else  
            Left(s"TypeError: no type for `$exp`")
        case LLet(x, _, e1, e2) => 
          val _t1 = typecheck(e1, env)
          val _t2 = _t1.flatMap(t1 => typecheck(e2, Map(x -> t1)))
          if _t1.isRight && _t2.isRight then
            _t2
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    