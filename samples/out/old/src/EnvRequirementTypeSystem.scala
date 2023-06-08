
    import tyes.runtime.old.*
    import tyes.runtime.Type
    import example.*
    
    object EnvRequirementTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case Int
        case Real
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case LNumber(_c1) => 
          if _c1 == 1 && env.size == 1 && env.get("pi") == Some(Type.Real) then
            Right(Type.Real)
          else  if _c1 == 3 && env.size == 1 && env.contains("pi") then
            Right(env("pi"))
          else  if _c1 == 4 then
            Right(Type.Real)
          else  if _c1 == 5 then
            Right(Type.Int)
          else  
            Left(s"TypeError: no type for `$exp`")
        case LPlus(e, _e2) => 
          val _t1 = typecheck(e, Map("pi" -> Type.Real))
          val _t2 = typecheck(e, Map("pi" -> Type.Int))
          val _t3 = typecheck(e, env)
          val _t4 = _t3.flatMap(t => typecheck(LNumber(1), Map("pi" -> t)))
          if _e2 == LNumber(1) then
            _t1
          else  if _e2 == LNumber(2) then
            _t2
          else  if _e2 == LNumber(3) then
            if _t3.isRight && _t4 == _t3 then
              _t3
            else
              Left(s"TypeError: no type for `$exp`")
          else  
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    