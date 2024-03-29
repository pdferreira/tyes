
    import tyes.runtime.old.*
    import tyes.runtime.Type
    import example.*
    
    object PlusOneTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case One
        case SumOne
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
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
    