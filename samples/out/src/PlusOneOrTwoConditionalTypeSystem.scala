
    import tyes.runtime.*
    import example.*
    
    object PlusOneOrTwoConditionalTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case One, Two, PlusTwo
    
      def typecheck(exp: LExpression): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  if _c1 == 2 then
            Right(Type.Two)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e1, e2) => 
          val _t1 = typecheck(e1)
          val _t2 = typecheck(e2)
          if _t1 == Right(Type.One) && _t2 == Right(Type.One) then
            Right(Type.One)
          else if _t2 == Right(Type.Two) then
            Right(Type.PlusTwo)
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    