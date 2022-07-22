package tyes.model

object TermConversions:

  given Conversion[Type, Term] with

    def apply(typ: Type): Term = typ match {
      case tn @ Type.Named(_) => Term.Constant(tn)
      case Type.Variable(name) => Term.Variable(name)
    }

  given [T](using Conversion[T, Term]): Conversion[Option[T], Term] with

    def apply(opt: Option[T]): Term = opt match {
      case None => Constants.Terms.any
      case Some(value) => value.convert
    }
