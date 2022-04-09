
    import tyes.runtime.*
    import example.*
    
    object PlusOneTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case One, SumOne
    
      def typecheck(exp: LExpression): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e, _e2) => 
          if _e2 == LNumber(1) then
            Right(Type.SumOne)
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    