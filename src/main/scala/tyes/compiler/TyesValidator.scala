package tyes.compiler

import tyes.model.*

object TyesValidator:
  private def getRuleDisplayName(rule: RuleDecl, tsDecl: TypeSystemDecl): String =
    val ruleName = rule.name match {
      case None => s"#${tsDecl.rules.indexOf(rule)}"
      case Some(n) => n.mkString("'", "", "'")
    }
    s"rule ${ruleName}"

  def validate(tsDecl: TypeSystemDecl): Seq[String] =
    Seq(
      validateAmbiguity(tsDecl),
      validateScope(tsDecl),
    ).flatten

  def validateScope(tsDecl: TypeSystemDecl): Seq[String] =
    (for
      r <- tsDecl.rules
      Judgement(_, HasType(ce, _)) = r.conclusion
      premiseVariables = r.premises.toSet.map { case Judgement(_, HasType(pe, _)) => pe.variables } .flatten
      unknownVariables = premiseVariables.diff(ce.variables)
      if !unknownVariables.isEmpty
    yield
      val ruleName = getRuleDisplayName(r, tsDecl)
      s"Error: ${ruleName} premises use some identifiers not in the conclusion: ${unknownVariables.mkString(", ")}"
    ).toSeq

  def validateAmbiguity(tsDecl: TypeSystemDecl): Seq[String] =
    (for
      case Seq(r1, r2) <- tsDecl.rules.combinations(2)
      Judgement(_, HasType(e1, t1)) = r1.conclusion
      Judgement(_, HasType(e2, t2)) = r2.conclusion
      if t1 != t2 && e1.overlaps(e2)
      // As a simplification, only perform the validation if there are no premises
      // TODO: replace this by a validation if the premises could ever hold true for the same term
      if r1.premises.isEmpty || r2.premises.isEmpty
    yield
      val r1Name = getRuleDisplayName(r1, tsDecl)
      val r2Name = getRuleDisplayName(r2, tsDecl)
      s"Error: conclusions of ${r1Name} and ${r2Name} overlap but result in different types"
    ).toSeq