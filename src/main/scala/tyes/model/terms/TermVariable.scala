package tyes.model.terms

trait TermVariable[+Self]:
  this: Self =>

  def name: String
  def copy(name: String = name): Self
