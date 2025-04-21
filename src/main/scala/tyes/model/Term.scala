package tyes.model

enum Term extends terms.TermOps[Term, Any](TermBuilder):
  case Constant[T](value: T)
  case Variable(name: String, index: Option[Term]) extends Term, terms.TermVariable
  case Function(name: String, args: Term*)
  case Type(typ: tyes.model.Type)

  private def ifBothTypes[B](otherTerm: Term, default: => B)(fn: (tyes.model.Type, tyes.model.Type) => B): Option[B] = (this, otherTerm) match {
    case (Term.Type(thisType), Term.Type(otherType)) => Some(fn(thisType, otherType))
    case (Term.Type(_), _) => Some(default)
    case (_, Term.Type(_)) => Some(default)
    case _ => None
  }

  private def ifType[B](fn: tyes.model.Type => B): Option[B] = this match {
    case Term.Type(typ) => Some(fn(typ))
    case _ => None
  }

  override def matches(otherTerm: Term): Option[Map[String, Term]] = 
    ifBothTypes(otherTerm, None) { (thisType, otherType) =>
      thisType.matches(otherType).map(_.mapValues(Term.Type.apply).toMap)
    }.getOrElse { 
      super.matches(otherTerm) 
    }

  override def overlaps(otherTerm: Term): Boolean = 
    ifBothTypes(otherTerm, false) { 
      _.overlaps(_) 
    }.getOrElse { 
      super.overlaps(otherTerm) 
    }

  override def isGround: Boolean = ifType(_.isGround).getOrElse { super.isGround }

  override def substitute(subst: Map[String, Term]): Term = 
    ifType { typ =>
      val typSubst = Map.from(for case (k, Term.Type(t)) <- subst yield k -> t)
      Term.Type(typ.substitute(typSubst))
    } getOrElse {
      super.substitute(subst)
    }

  override def unifies(otherTerm: Term): Option[Map[String, Term]] =
    ifBothTypes(otherTerm, None) { (thisType, otherType) =>
      thisType.unifies(otherType).map(_.mapValues(Term.Type.apply).toMap)
    }.getOrElse { 
      super.unifies(otherTerm) 
    }

  override def variables: Set[String] = ifType(_.variables).getOrElse { super.variables }

  override def toString(): String = ifType(typ => s"T:$typ").getOrElse { super.toString }

private object TermBuilder extends terms.TermBuilder[Term, Any]:
  
  override def applyConstant(value: Any): Term = Term.Constant(value)

  override def unapplyConstant(term: Term): Option[Any] = term match {
    case Term.Constant(v) => Some(v)
    case _ => None
  }

  override def applyVariable(name: String): Term & terms.TermVariable = name.split("_", 2) match {
    case Array(varName, idxStr) =>
      val idxTerm =
        if raw"\d+".r.matches(idxStr)
        then Term.Constant(idxStr.toInt)
        else Term.Variable(idxStr, index = None)

      Term.Variable(varName, index = Some(idxTerm)) 
    case _ => Term.Variable(name, index = None)
  }

  override def unapplyVariable(term: Term): Option[String] = term match {
    case Term.Variable(name, None) => Some(name)
    case Term.Variable(name, Some(Term.Constant(idxValue: Int))) => Some(s"${name}_${idxValue}")
    case Term.Variable(name, Some(Term.Variable(idxVarName, None))) => Some(s"${name}_${idxVarName}")
    case _ => None
  }

  override def applyFunction(name: String, args: Term*): Term = Term.Function(name, args*)
  
  override def unapplyFunction(term: Term): Option[(String, Seq[Term])] = term match {
    case Term.Function(name, args*) => Some((name, args))
    case _ => None
  }
