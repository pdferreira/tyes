package tyes.model.terms

trait TermRange[TTerm <: TermOps[TTerm, TConstant], TConstant]:
  def function: String
  def cursor: String
  def template: TTerm
  def minIndex: Int
  def maxIndex: Either[String, Int]
  def seed: Option[TTerm]