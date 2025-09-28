package tyes.model.terms

trait TermBuilder[TTerm, TConstant]:

  def applyVariable(name: String): TTerm & TermVariable
  
  def unapplyVariable(term: TTerm): Option[String]

  def applyConstant(value: TConstant): TTerm

  def unapplyConstant(term: TTerm): Option[TConstant]

  def applyFunction(name: String, args: TTerm*): TTerm
  
  def unapplyFunction(term: TTerm): Option[(String, Seq[TTerm])]

  def applyRange(
    function: String,
    cursor: String,
    holeArgIdx: Int,
    argTemplates: Seq[TTerm],
    minIndex: Int,
    maxIndex: Index,
    holeIsMax: Boolean,
    holeSeed: Option[TTerm] = None,
  ): TTerm & TermRange[TTerm]

  def unapplyRange(term: TTerm): Option[(
    String,
    String,
    Int,
    Seq[TTerm],
    Int,
    Index,
    Boolean,
    Option[TTerm]
  )]
