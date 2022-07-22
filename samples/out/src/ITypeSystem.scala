
    import tyes.runtime.*
    import example.*
    
    object ITypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case Int
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.Int)
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    