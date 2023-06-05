
    import tyes.runtime.*
    import example.*
    
    object LambdaSimRecTypeSystem extends TypeSystem[LExpression], TypeOperations:
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
        case LVariable(f) => 
          val Seq(_t2, _t) = destructure[Type.$FunType](env.get(f), s"'$f' is not in scope")
          if _t2.isRight && _t.isRight then
            Right(Type.$FunType(_t.getOrElse(???), _t2.getOrElse(???)))
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e1, e2) => 
          val _t1 = typecheck(e1, env).flatMap(cast[Type.$FunType])
          val _t2 = typecheck(e2, env)
          if _t1.isRight && _t2 == _t1.map(_.t1) then
            _t1.map(_.t2)
          else 
            Left(s"TypeError: no type for `$exp`")
        case LLet(f, _ct2, e, _e4) => 
          val Seq(_t, _t2) = destructure[Type.$FunType](_ct2)
          val _t1 = _t2.flatMap(t2 => _t.flatMap(t => typecheck(e, env ++ Map(f -> Type.$FunType(t2, t))))).flatMap(cast[Type.$FunType])
          if _e4 == LVariable("rec") && _t == _t1.map(_.t1) && _t2 == _t1.map(_.t2) then
            if _t1.isRight then
              Right(Type.$FunType(_t.getOrElse(???), _t2.getOrElse(???)))
            else
              Left(s"TypeError: no type for `$exp`")
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    