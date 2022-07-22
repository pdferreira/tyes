
    import tyes.runtime.*
    import example.*
    
    object SumAccVarTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case One, Two, Three
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  if _c1 == 2 then
            Right(Type.Two)
          else  if _c1 == 3 then
            Right(Type.Three)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LVariable(x) => 
          if env.size == 1 && env.contains(x) then
            Right(env(x))
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e1, e2) => 
          val _t1 = typecheck(e1, env)
          val _t2 = _t1.flatMap(t1 => typecheck(e2, Map("acc" -> t1)))
          if _t1.isRight && _t2.isRight then
            _t2
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    