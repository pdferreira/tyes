
    import tyes.runtime.old.*
    import tyes.runtime.Type
    import example.*
    
    object OneOrTwoTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case One
        case Two
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  if _c1 == 2 then
            Right(Type.Two)
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    