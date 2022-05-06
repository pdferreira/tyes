
    import tyes.runtime.*
    import example.*
    
    object OneOrTwoTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case One, Two
    
      def typecheck(exp: LExpression): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  if _c1 == 2 then
            Right(Type.Two)
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    