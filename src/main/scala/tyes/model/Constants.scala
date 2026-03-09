package tyes.model

object Constants:
  
  object Types:

    val any: Type.Variable = Type.Variable("$any")

    object Function:

      val name = "$FunType"

      val operator = "->"
      
      def apply(arg: Type, ret: Type) = Type.Composite(name, arg, ret)
      
      def unapply(typ: Type) = PartialFunction.condOpt[Type, (Type, Type)](typ) {
        case Type.Composite(`name`, arg, ret) => (arg, ret)
      }

    object Record:

      object Empty:

        val name = "$EmptyRecord"

        def apply() = Type.Named(name)

        def unapply(typ: Type) = PartialFunction.cond[Type](typ) {
          case Type.Named(`name`) => true
        }

      val name = "$Record"

      val delimiterL = "{"

      val delimiterR = "}"

      def apply(fields: Seq[(Label, Type)]): Type = fields.foldRight(Empty()) {
        case ((field, typ), rest) => Type.Composite(name, Type.Label(field), typ, rest)
      }

      def unapply(typ: Type): Option[Seq[(Label, Type)]] = typ match {
        case Empty() => Some(Seq())
        case Type.Composite(`name`, Type.Label(field), typ, rest) =>
          unapply(rest).map((field, typ) +: _)
        case _ => None 
      }
