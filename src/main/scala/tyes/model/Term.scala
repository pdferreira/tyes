package tyes.model

enum Term extends terms.TermOps[Term, Any](TermBuilder):
  case Constant[T](value: T)
  case Variable(name: String)
  case Function(name: String, args: Term*)

private object TermBuilder extends terms.TermBuilder[Term, Any]:
  
  override def applyConstant(value: Any): Term = Term.Constant(value)

  override def unapplyConstant(term: Term): Option[Any] = term match {
    case Term.Constant(v) => Some(v)
    case _ => None
  }

  override def applyVariable(name: String): Term = Term.Variable(name)

  override def unapplyVariable(term: Term): Option[String] = term match {
    case Term.Variable(name) => Some(name)
    case _ => None
  }

  override def applyFunction(name: String, args: Term*): Term = Term.Function(name, args*)
  
  override def unapplyFunction(term: Term): Option[(String, Seq[Term])] = term match {
    case Term.Function(name, args*) => Some((name, args))
    case _ => None
  }
