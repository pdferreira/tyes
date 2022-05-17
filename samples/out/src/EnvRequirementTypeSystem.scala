
    import tyes.runtime.*
    import example.*
    
    object EnvRequirementTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case Real
    
      def typecheck(exp: LExpression, env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 && env.contains("pi") && env("pi") == Type.Real then
            Right(Type.Real)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e, _e2) => 
          val _t1 = typecheck(e, env + ("pi" -> Type.Real))
          if _e2 == LNumber(1) then
            _t1
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    