package tyes.model

import tyes.model.Type
import tyes.model.ranges.*

enum Term extends terms.TermOps[Term, Any](TermBuilder) with TypeVariableContainer:
  case Constant[T](value: T)
  case Variable(name: String) extends Term, terms.TermVariable
  case Function(name: String, args: Term*)
  case Type(typ: tyes.model.Type)
  case Label(label: tyes.model.Label)
  case Range(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[Term],
    minIndex: Int,
    maxIndex: terms.Index,
    holeSeed: Option[Term] = None,
  ) extends Term, terms.TermRange[Term]

  override def matches(otherTerm: Term): Option[Map[String, Term]] = (this, otherTerm) match { 
    case (Term.Type(thisType), Term.Type(otherType)) =>
      thisType.matches(otherType).map(_.view.mapValues(Term.Type.apply).toMap)
    case (Term.Label(thisLabel), Term.Label(otherLabel)) =>
      thisLabel.matches(otherLabel).map(_.view.mapValues(Term.Label.apply).toMap)
    case _ => 
      super.matches(otherTerm) 
  }

  override def overlaps(otherTerm: Term): Boolean = (this, otherTerm) match { 
    case (Term.Type(thisType), Term.Type(otherType)) => thisType.overlaps(otherType)
    case (Term.Label(thisLabel), Term.Label(otherLabel)) => thisLabel.overlaps(otherLabel)
    case _ => super.overlaps(otherTerm) 
  }

  override def isGround: Boolean = this match {
    case Term.Type(typ) => typ.isGround
    case Term.Label(label) => label.isGround
    case _ => super.isGround 
  }

  override def substitute(subst: Map[String, Term]): Term = this match { 
    case Term.Type(typ) =>
      val typSubst = TyesLanguageExtensions.termSubstToTypeSubst(subst)
      Term.Type(typ.substitute(typSubst))
    case Term.Label(label) =>
      val labelSubst = TyesLanguageExtensions.termSubstToLabelSubst(subst)
      Term.Label(label.substitute(labelSubst))
    case _ =>
      super.substitute(subst)
  }

  override def unifies(otherTerm: Term): Option[Map[String, Term]] = (this, otherTerm) match {
    case (Term.Type(thisType), Term.Type(otherType)) =>
      thisType.unifies(otherType).map(_.view.mapValues(Term.Type.apply).toMap)
    case (Term.Label(thisLabel), Term.Label(otherLabel)) =>
      thisLabel.unifies(otherLabel).map(_.view.mapValues(Term.Label.apply).toMap)
    case _ => 
      super.unifies(otherTerm) 
  }

  override def replaceIndex(oldIdxStr: String, newIdxStr: String): Term = this match {
    case Term.Type(typ) => Term.Type(typ.replaceIndex(oldIdxStr, newIdxStr))
    case Term.Label(label) => Term.Label(label.replaceIndex(oldIdxStr, newIdxStr))
    case _ => super.replaceIndex(oldIdxStr, newIdxStr)
  }

  override def variables: Set[String] = this match {
    case Term.Type(typ) => typ.variables
    case Term.Label(label) => label.variables
    case _ => super.variables
  }

  def types: Set[tyes.model.Type] = this match {
    case Term.Constant(_) => Set()
    case Term.Variable(_) => Set()
    case Term.Function(_, args*) => args.flatMap(_.types).toSet
    case Term.Type(t) => Set(t)
    case Term.Label(_) => Set()
    case r: Term.Range => getRangeElems(r, _.types).toSet
  }

  def labels: Set[tyes.model.Label] = this match {
    case Term.Constant(_) => Set()
    case Term.Variable(_) => Set()
    case Term.Function(_, args*) => args.flatMap(_.labels).toSet
    case Term.Type(t) => t.labels
    case Term.Label(l) => Set(l)
    case r: Term.Range => getRangeElems(r, _.labels).toSet
  }

  override def typeVariables: Iterable[tyes.model.Type.Variable] = types.flatMap(_.typeVariables)

  override def toString(): String = this match {
    case Term.Type(typ) => s"T:$typ"
    case Term.Label(label) => s"L:$label"
    case _ => super.toString
  }

object TermBuilder extends terms.TermBuilder[Term, Any]:
  
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
    holeSeed: Option[Term] = None
  ): Term & terms.TermRange[Term] =
    Term.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeSeed)

  override def unapplyRange(term: Term): Option[(
    String,
    String,
    Int,
    Seq[Term],
    Int,
    terms.Index,
    Option[Term]
  )] = term match {
    case Term.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeSeed) =>
      Some((function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeSeed))
    case _ => None
  }
