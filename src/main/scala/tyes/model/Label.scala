package tyes.model

enum Label extends terms.TermOps[Label, String](LabelBuilder):
  case Constant(name: String)
  case Variable(name: String) extends Label, terms.TermVariable

  def labelVariables: Iterable[Label.Variable] = this match {
    case _: Constant => Set()
    case v: Variable => Set(v)
  }

private object LabelBuilder extends terms.TermBuilder[Label, String]:

  override def applyConstant(value: String): Label = Label.Constant(value)

  override def unapplyConstant(term: Label): Option[String] = term match {
    case Label.Constant(name) => Some(name)
    case _ => None
  }

  override def applyVariable(name: String): Label & terms.TermVariable = Label.Variable(name)

  override def unapplyVariable(term: Label): Option[String] = term match {
    case Label.Variable(name) => Some(name)
    case _ => None
  }

  override def applyFunction(name: String, args: Label*): Label = throw new NotImplementedError("not supported")

  override def unapplyFunction(term: Label): Option[(String, Seq[Label])] = None

  override def applyRange(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[Label],
    minIndex: Int,
    maxIndex: terms.Index,
    holeSeed: Option[Label] = None
  ): Label & terms.TermRange[Label] = throw new NotImplementedError("not supported")

  override def unapplyRange(term: Label): Option[(
    String,
    String,
    Int,
    Seq[Label],
    Int,
    terms.Index,
    Option[Label]
  )] = None
