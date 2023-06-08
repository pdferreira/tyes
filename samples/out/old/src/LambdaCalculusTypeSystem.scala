
    import tyes.runtime.old.*
    import tyes.runtime.Type
    import example.*
    
    object LambdaCalculusTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case Const
        case $FunType(t1: Type, t2: Type) extends Type, tyes.runtime.CompositeType(t1, t2)
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 then
            Right(Type.Const)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LVariable(x) => 
          if env.contains(x) then
            Right(env(x))
          else  
            Left(s"TypeError: no type for `$exp`")
        case LApp(e1, e2) => 
          val _t1 = typecheck(e1, env).flatMap(cast[Type.$FunType])
          val _t2 = typecheck(e2, env)
          if _t1.isRight && _t2 == _t1.map(_.t1) then
            _t1.map(_.t2)
          else 
            Left(s"TypeError: no type for `$exp`")
        case LFun(x, t, e) => 
          val _t1 = t.toRight("No type provided").flatMap(_t => typecheck(e, env ++ Map(x -> _t)))
          if _t1.isRight then
            Right(Type.$FunType(t.get, _t1.getOrElse(???)))
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    