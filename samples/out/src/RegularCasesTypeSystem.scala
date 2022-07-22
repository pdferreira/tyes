
    import tyes.runtime.*
    import example.*
    
    object RegularCasesTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case One
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e1, e2) => 
          val _t1 = typecheck(e1, env)
          val _t2 = typecheck(e2, env)
          if _t1 == Right(Type.One) && _t2 == Right(Type.One) then
            Right(Type.One)
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    