package tyes.model

enum Type extends terms.TermOps[Type, String](TypeBuilder):
  case Named(name: String)
  case Variable(name: String) extends Type, terms.TermVariable
  case Composite(name: String, args: Type*)
  case Range(
    function: String,
    cursor: String,
    template: Type,
    minIndex: Int,
    maxIndex: Either[String, Int],
    seed: Option[Type] = None
  )

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
    template: Type,
    minIndex: Int,
    maxIndex: Either[String, Int],
    seed: Option[Type] = None
  ): Type = Type.Range(function, cursor, template, minIndex, maxIndex, seed)

  override def unapplyRange(term: Type): Option[(
    String,
    String,
    Type,
    Int,
    Either[String, Int],
    Option[Type]
  )] = term match {
    case Type.Range(function, cursor, template, minIndex, maxIndex, seed) =>
      Some((function, cursor, template, minIndex, maxIndex, seed))
    case _ => None
  }