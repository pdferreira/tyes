
    import tyes.runtime.*
    import example.*
    
    object EnvRequirementTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case Real, Int
    
      def typecheck(exp: LExpression, env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 && env.contains("pi") && env("pi") == Type.Real then
            Right(Type.Real)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e, _e2) => 
          val _t1 = typecheck(e, env + ("pi" -> Type.Real))
          val _t2 = typecheck(e, env + ("pi" -> Type.Int))
          if _e2 == LNumber(1) then
            _t1
          else  if _e2 == LNumber(2) then
            _t2
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    