
    import tyes.runtime.*
    import example.*
    
    object LambdaSimTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case One, Two
    
      def typecheck(exp: LExpression, env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.One)
          else  if _c1 == 2 then
            Right(Type.Two)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LLet(x, t: Option[Type], e1, e2) => 
          val _t1 = t.toRight("No type provided").flatMap(_t => typecheck(e2, Map(x -> _t)))
          if _t1.isRight then
            _t1
          else 
            Left(s"TypeError: no type for `$exp`")
        case LVariable(x) => 
          if env.size == 1 && env.contains(x) then
            Right(env(x))
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    