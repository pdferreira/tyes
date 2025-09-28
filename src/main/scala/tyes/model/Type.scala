package tyes.model

enum Type extends terms.TermOps[Type, String](TypeBuilder):
  case Named(name: String)
  case Variable(name: String) extends Type, terms.TermVariable
  case Composite(name: String, args: Type*)
  case Range(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[Type],
    minIndex: Int,
    maxIndex: terms.Index,
    holeIsMax: Boolean,
    holeSeed: Option[Type] = None
  ) extends Type, terms.TermRange[Type]

private object TypeBuilder extends terms.TermBuilder[Type, String]:

  override def applyConstant(value: String): Type = Type.Named(value)

  override def unapplyConstant(term: Type): Option[String] = term match {
    case Type.Named(name) => Some(name)
    case _ => None
  }

  override def applyVariable(name: String): Type & terms.TermVariable = Type.Variable(name)

  override def unapplyVariable(term: Type): Option[String] = term match {
    case Type.Variable(name) => Some(name)
    case _ => None
  }

  override def applyFunction(name: String, args: Type*): Type = Type.Composite(name, args*)

  override def unapplyFunction(term: Type): Option[(String, Seq[Type])] = term match {
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
    holeIsMax: Boolean,
    holeSeed: Option[Type] = None
  ): Type & terms.TermRange[Type] = Type.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed)

  override def unapplyRange(term: Type): Option[(
    String,
    String,
    Int,
    Seq[Type],
    Int,
    terms.Index,
    Boolean,
    Option[Type]
  )] = term match {
    case Type.Range(function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed) =>
      Some((function, cursor, holeArgIdx, argTemplates, minIndex, maxIndex, holeIsMax, holeSeed))
    case _ => None
  }