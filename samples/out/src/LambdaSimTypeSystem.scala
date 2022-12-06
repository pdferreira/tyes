
    import tyes.runtime.*
    import example.*
    
    object LambdaSimTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case One
        case Two
        case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)
    
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
          val _t1 = typecheck(e1, env).flatMap(cast[Type.$FunType])
          val _t2 = typecheck(e2, env)
          if _t1.isRight && _t2 == _t1.map(_.t1) then
            _t1.map(_.t2)
          else 
            Left(s"TypeError: no type for `$exp`")
        case LLet(x, t, e, _e4) => 
          val _t1 = t.toRight("No type provided").flatMap(_t => typecheck(e, Map(x -> _t)))
          if _e4 == LVariable("noop") then
            if _t1.isRight then
              Right(Type.$FunType(t.get, _t1.getOrElse(???)))
            else
              Left(s"TypeError: no type for `$exp`")
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    