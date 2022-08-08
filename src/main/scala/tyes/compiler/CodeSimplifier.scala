package tyes.compiler

object CodeSimplifier:
    
  private val simplificationRules: Seq[String => String] = Seq(
    raw"^Right\(([^ ]+)\.getOrElse\(.+\)\)$$".r.replaceAllIn(_, m => m.group(1)),
    raw"^Right\([^ ]+\)\.isRight$$".r.replaceAllIn(_, "true"),
  )

  def simplify(codeStr: String): String =
    simplificationRules.foldLeft(codeStr) { (currStr, currRule) => currRule(currStr) }
