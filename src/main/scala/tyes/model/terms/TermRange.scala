package tyes.model.terms

trait TermRange[TTerm]:
  def function: String
  def cursor: String
  def holeArgIdx: Int
  def argTemplates: Seq[TTerm]
  def minIndex: Int
  def maxIndex: Index
  def holeSeed: Option[TTerm]

  def replaceIndex(oldIdxStr: String, newIdxStr: String): TTerm

  if holeArgIdx < 0 || holeArgIdx > argTemplates.size then
    throw new IllegalStateException("`holeArgIdx` must be a valid index")
  else if argTemplates.size != 1 && holeSeed.isEmpty then
    throw new IllegalStateException("`holeSeed` must be provided when the function arity is above 2")