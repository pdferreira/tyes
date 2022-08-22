package tyes.model

object Constants:
  
  object Types:

    val any = Type.Variable("$any")

    object Function:

      val name = "$FunType"

      val operator = "->"
      
      def apply(arg: Type, ret: Type) = Type.Composite(name, arg, ret)
      
      def unapply(typ: Type) = PartialFunction.condOpt[Type, (Type, Type)](typ) {
        case Type.Composite(tname, arg, ret) if tname == name => (arg, ret)
      }
    
