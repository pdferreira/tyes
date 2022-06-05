
    import tyes.runtime.*
    import example.*
    
    object TermVarTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case Sumpi, Real
    
      def typecheck(exp: LExpression, env: Map[String, Type]): Either[String, Type] = exp match {
        case LPlus(_e1, _e2) => 
          val _t1 = typecheck(LVariable("pi"), Map("pi" -> Type.Real))
          if _e2 == LNumber(1) && _e1 == LVariable("pi") then
            if _t1 == Right(Type.Real) then
              Right(Type.Sumpi)
            else
              Left(s"TypeError: no type for `$exp`")
          else  
            Left(s"TypeError: no type for `$exp`")
        case LVariable(_c1) => 
          if _c1 == "pi" && env.size == 1 && env.contains("pi") then
            Right(env("pi"))
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    