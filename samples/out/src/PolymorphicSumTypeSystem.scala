
    import tyes.runtime.*
    import example.*
    
    object PolymorphicSumTypeSystem extends TypeSystem[LExpression]:
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
        case LPlus(e1, e2) => 
          val _t1 = typecheck(e1)
          val _t2 = typecheck(e2)
          if _t1.isRight && _t2 == _t1 then
            _t1
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    