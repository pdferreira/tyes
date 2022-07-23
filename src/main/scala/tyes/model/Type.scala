package tyes.model

enum Type extends terms.TermOps[Type, String](TypeBuilder):
  case Named(name: String)
  case Variable(name: String)

private object TypeBuilder extends terms.TermBuilder[Type, String]:

  override def applyConstant(value: String): Type = Type.Named(value)

  override def unapplyConstant(term: Type): Option[String] = term match {
    case Type.Named(name) => Some(name)
    case _ => None
  }

  override def applyVariable(name: String): Type = Type.Variable(name)

  override def unapplyVariable(term: Type): Option[String] = term match {
    case Type.Variable(name) => Some(name)
    case _ => None
  }

  override def applyFunction(name: String, args: Type*): Type = ???

  override def unapplyFunction(term: Type): Option[(String, Seq[Type])] = ???
