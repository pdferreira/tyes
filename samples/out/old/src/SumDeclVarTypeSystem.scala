
    import tyes.runtime.*
    import example.*
    
    object SumDeclVarTypeSystem extends TypeSystem[LExpression], TypeOperations:
      type T = Type
    
      enum Type extends tyes.runtime.Type:
        case One
        case Three
        case Two
    
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
        case LPlus(_e1, e) => 
          val _x = _e1 match { case LVariable(v) => Right(v) ; case _ => Left("Not a LVariable") }
          val _t1 = _x.flatMap(x => typecheck(e, Map(x -> Type.Two)))
          if _t1.isRight then
            _t1
          else 
            Left(s"TypeError: no type for `$exp`")
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    