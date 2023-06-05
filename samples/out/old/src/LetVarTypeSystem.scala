
    import tyes.runtime.*
    import example.*
    
    object LetVarTypeSystem extends TypeSystem[LExpression], TypeOperations:
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
        case LVariable(x) => 
          if env.size == 1 && env.contains(x) then
            Right(env(x))
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e1, e2) => 
          val _t1 = typecheck(e1, env)
          val _t2 = typecheck(e2, env)
          if _t1.isRight && _t2 == _t1 then
            _t1
          else 
            Left(s"TypeError: no type for `$exp`")
        case LLet(x, t1, e1, e2) => 
          val _t1 = typecheck(e1, env)
          val _t2 = t1.toRight("No type provided").flatMap(_t1 => typecheck(e2, Map(x -> _t1)))
          if t1.toRight("No type provided") == _t1 then
            if _t1.isRight && _t2.isRight then
              _t2
            else
              Left(s"TypeError: no type for `$exp`")
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    