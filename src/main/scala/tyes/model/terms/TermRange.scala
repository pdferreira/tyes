package tyes.model.terms

trait TermRange[TTerm]:
  def function: String
  def cursor: String
  def template: TTerm
  def minIndex: Int
  def maxIndex: Index
  def seed: Option[TTerm]

  def replaceIndex(oldIdxStr: String, newIdxStr: String): TTerm