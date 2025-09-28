package tyes.model

enum Term extends terms.TermOps[Term, Any](TermBuilder):
  case Constant[T](value: T)
  case Variable(name: String) extends Term, terms.TermVariable
  case Function(name: String, args: Term*)
  case Type(typ: tyes.model.Type)
  case Range(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[Term],
    minIndex: Int,
    maxIndex: terms.Index,
    holeIsMax: Boolean,
    holeSeed: Option[Term] = None,
  ) extends Term, terms.TermRange[Term]

  private def ifBothTypes[B](otherTerm: Term)(fn: (tyes.model.Type, tyes.model.Type) => B): Option[B] = (this, otherTerm) match {
    case (Term.Type(thisType), Term.Type(otherType)) => Some(fn(thisType, otherType))
    case _ => None
  }

  private def ifType[B](fn: tyes.model.Type => B): Option[B] = this match {
    case Term.Type(typ) => Some(fn(typ))
    case _ => None
  }

  override def matches(otherTerm: Term): Option[Map[String, Term]] = 
    ifBothTypes(otherTerm) { (thisType, otherType) =>
      thisType.matches(otherType).map(_.view.mapValues(Term.Type.apply).toMap)
    }.getOrElse { 
      super.matches(otherTerm) 
    }

  override def overlaps(otherTerm: Term): Boolean = 
    ifBothTypes(otherTerm) { 
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
    ifBothTypes(otherTerm) { (thisType, otherType) =>
      thisType.unifies(otherType).map(_.view.mapValues(Term.Type.apply).toMap)
    }.getOrElse { 
      super.unifies(otherTerm) 
    }

  override def replaceIndex(oldIdxStr: String, newIdxStr: String): Term = 
    ifType { typ =>
      Term.Type(typ.replaceIndex(oldIdxStr, newIdxStr))
    } getOrElse {
      super.replaceIndex(oldIdxStr, newIdxStr)
    }

  override def variables: Set[String] = ifType(_.variables).getOrElse { super.variables }

  override def toString(): String = ifType(typ => s"T:$typ").getOrElse { super.toString }

private object TermBuilder extends terms.TermBuilder[Term, Any]:
  
  override def applyConstant(value: Any): Term = Term.Constant(value)

  override def unapplyConstant(term: Term): Option[Any] = term match {
    case Term.Constant(v) => Some(v)
    case _ => None
  }

  override def applyVariable(name: String): Term & terms.TermVariable = Term.Variable(name)

  override def unapplyVariable(term: Term): Option[String] = term match {
    case Term.Variable(name) => Some(name)
    case _ => None
  }

  override def applyFunction(name: String, args: Term*): Term = Term.Function(name, args*)
  
  override def unapplyFunction(term: Term): Option[(String, Seq[Term])] = term match {
    case Term.Function(name, args*) => Some((name, args))
    case _ => None
  }

  override def applyRange(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[Term],
    minIndex: Int,
    maxIndex: terms.Index,
    holeIsMax: Boolean,
    holeSeed: Option[Term] = None
  ): Term & terms.TermRange[Term] =
    Term.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed)

  override def unapplyRange(term: Term): Option[(
    String,
    String,
    Int,
    Seq[Term],
    Int,
    terms.Index,
    Boolean,
    Option[Term]
  )] = term match {
    case Term.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed) =>
      Some((function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed))
    case _ => None
  }
