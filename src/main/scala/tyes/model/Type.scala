package tyes.model

import tyes.model.ranges.*

enum Type extends terms.TermOps[Type, String](TypeBuilder) with TypeVariableContainer:
  case Named(name: String)
  case Label(label: tyes.model.Label)
  case Variable(name: String) extends Type, terms.TermVariable
  case Composite(name: String, args: Type*)
  case Range(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[Type],
    minIndex: Int,
    maxIndex: terms.Index,
    holeSeed: Option[Type] = None
  ) extends Type, terms.TermRange[Type]

  override def matches(otherType: Type): Option[Map[String, Type]] = (this, otherType) match { 
    case (Type.Label(thisLabel), Type.Label(otherLabel)) =>
      thisLabel.matches(otherLabel).map(_.view.mapValues(Type.Label.apply).toMap)
    case _ => 
      super.matches(otherType) 
  }

  override def overlaps(otherType: Type): Boolean = (this, otherType) match { 
    case (Type.Label(thisLabel), Type.Label(otherLabel)) => thisLabel.overlaps(otherLabel)
    case _ => super.overlaps(otherType) 
  }

  override def isGround: Boolean = this match {
    case Type.Label(label) => label.isGround
    case _ => super.isGround 
  }

  override def substitute(subst: Map[String, Type]): Type = this match { 
    case Type.Label(label) =>
      val labelSubst = subst.collect({ case (k, Type.Label(l)) => k -> l })
      Type.Label(label.substitute(labelSubst))
    case _ =>
      super.substitute(subst)
  }

  override def unifies(otherType: Type): Option[Map[String, Type]] = (this, otherType) match {
    case (Type.Label(thisLabel), Type.Label(otherLabel)) =>
      thisLabel.unifies(otherLabel).map(_.view.mapValues(Type.Label.apply).toMap)
    case _ => 
      super.unifies(otherType) 
  }

  override def replaceIndex(oldIdxStr: String, newIdxStr: String): Type = this match {
    case Type.Label(label) => Type.Label(label.replaceIndex(oldIdxStr, newIdxStr))
    case _ => super.replaceIndex(oldIdxStr, newIdxStr)
  }

  override def variables: Set[String] = this match {
    case Type.Label(label) => label.variables
    case _ => super.variables
  }

  override def typeVariables: Iterable[Type.Variable] = this match {
    case Type.Named(_) => Set.empty
    case Type.Label(_) => Set.empty
    case v: Type.Variable => Set(v)
    case Type.Composite(_, args*) => args.flatMap(_.typeVariables).toSet
    case r: Type.Range => getRangeElems(r, _.typeVariables).toSet
  }

  def labelVariables: Iterable[tyes.model.Label.Variable] = this match {
    case Type.Named(_) => Set.empty
    case Type.Variable(_) => Set.empty
    case Type.Label(l) => l.labelVariables
    case Type.Composite(_, args*) => args.flatMap(_.labelVariables).toSet
    case r: Type.Range => getRangeElems(r, _.labelVariables).toSet
  }

  def labels: Set[tyes.model.Label] = this match {
    case Type.Named(_) => Set.empty
    case Type.Variable(_) => Set.empty
    case Type.Label(l) => Set(l)
    case Type.Composite(_, args*) => args.flatMap(_.labels).toSet
    case r: Type.Range => getRangeElems(r, _.labels).toSet
  }

  override def toString(): String = this match {
    case Type.Label(label) => s"L:$label"
    case _ => super.toString
  }

object TypeBuilder extends terms.TermBuilder[Type, String]:

  override def applyConstant(value: String): Type = Type.Named(value)

  override def unapplyConstant(typ: Type): Option[String] = typ match {
    case Type.Named(name) => Some(name)
    case _ => None
  }

  override def applyVariable(name: String): Type & terms.TermVariable = Type.Variable(name)

  override def unapplyVariable(typ: Type): Option[String] = typ match {
    case Type.Variable(name) => Some(name)
    case _ => None
  }

  override def applyFunction(name: String, args: Type*): Type = Type.Composite(name, args*)

  override def unapplyFunction(typ: Type): Option[(String, Seq[Type])] = typ match {
    case Type.Composite(name, args*) => Some((name, args))
    case _ => None
  }

  override def applyRange(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[Type],
    minIndex: Int,
    maxIndex: terms.Index,
    holeSeed: Option[Type] = None
  ): Type & terms.TermRange[Type] = Type.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeSeed)

  override def unapplyRange(typ: Type): Option[(
    String,
    String,
    Int,
    Seq[Type],
    Int,
    terms.Index,
    Option[Type]
  )] = typ match {
    case Type.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeSeed) =>
      Some((function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeSeed))
    case _ => None
  }