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
    template: TTerm,
    minIndex: Int,
    maxIndex: Either[String, Int],
    seed: Option[TTerm] = None
  ): TTerm

  def unapplyRange(term: TTerm): Option[(
    String,
    String,
    TTerm,
    Int,
    Either[String, Int],
    Option[TTerm]
  )]
