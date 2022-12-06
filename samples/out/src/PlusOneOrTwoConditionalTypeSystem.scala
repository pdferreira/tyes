
    import tyes.runtime.*
    import example.*
    
    object PlusOneOrTwoConditionalTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case One
        case PlusTwo
        case Two
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  if _c1 == 2 then
            Right(Type.Two)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e1, e2) => 
          val _t1 = typecheck(e1, env)
          val _t2 = typecheck(e2, env)
          val _t3 = typecheck(e2, env)
          if _t1 == Right(Type.One) && _t2 == Right(Type.One) then
            Right(Type.One)
          else if _t3 == Right(Type.Two) then
            Right(Type.PlusTwo)
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    