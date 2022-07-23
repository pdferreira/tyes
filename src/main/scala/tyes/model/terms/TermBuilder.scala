package tyes.model.terms

trait TermBuilder[TTerm, TConstant]:

  def applyVariable(name: String): TTerm
  
  def unapplyVariable(term: TTerm): Option[String]

  def applyConstant(value: TConstant): TTerm

  def unapplyConstant(term: TTerm): Option[TConstant]

  def applyFunction(name: String, args: TTerm*): TTerm
  
  def unapplyFunction(term: TTerm): Option[(String, Seq[TTerm])]
