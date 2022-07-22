
    import tyes.runtime.*
    import example.*
    
    object AnyTypeSystem extends TypeSystem[LExpression]:
      type T = Type
    
      enum Type:
        case Any
    
      def typecheck(exp: LExpression[Type], env: Map[String, Type]): Either[String, Type] = exp match {
        case e => Right(Type.Any)
        case _ => Left(s"TypeError: no type for `$exp`")
      }
    