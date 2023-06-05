
    import tyes.runtime.*
    import example.*
    
    object MetaVarTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case Int
        case Real
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 0 then
            Right(Type.Int)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LVariable(_c1) => 
          if _c1 == "pi" then
            Right(Type.Real)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(_e1, _e2) => 
          val _x = _e1 match { case LVariable(v) => Right(v) ; case _ => Left("Not a LVariable") }
          val _t1 = _x.flatMap(x => typecheck(LVariable(x), env))
          if _e2 == LNumber(1) then
            _t1
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    