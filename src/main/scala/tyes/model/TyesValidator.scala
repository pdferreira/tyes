package tyes.model

import scala.collection.mutable.ListBuffer
import tyes.model.*
import tyes.model.indexes.*
import tyes.model.scope.*
import tyes.model.TyesLanguageExtensions.*
import utils.collections.*

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
      validateStructure(tsDecl),
      validateScope(tsDecl),
    ).flatten 

  def validateScope(tsDecl: TypeSystemDecl): Seq[String] =
    val errors = ListBuffer.empty[String]
    for r <- tsDecl.rules do
      val ruleName = getRuleDisplayName(r, tsDecl)
      val concl = r.conclusion

      val psTermVariables = r.premises.flatMap(_.termVariables).toSet
      val unknownPsTermVariables = psTermVariables.diff(concl.termVariables)
      if !unknownPsTermVariables.isEmpty then
        errors += s"Error: $ruleName premises use some identifiers not bound in the conclusion: ${unknownPsTermVariables.mkString(", ")}"

      val conclEnvTermVariables = concl.env.termVariables
      val unknownConclEnvTermVariables = conclEnvTermVariables.diff(concl.assertion.termVariables)
      if !unknownConclEnvTermVariables.isEmpty then
        errors += s"Error: $ruleName conclusion environment uses some identifiers not bound in its term: ${unknownConclEnvTermVariables.mkString(", ")}"
        
      val HasType(conclTerm, conclTyp) = concl.assertion: @unchecked
      for cTypeVar <- conclTyp.variables do
        if conclTerm.types.exists(t => t.variables.contains(cTypeVar)) then
          () // ok, bound in concl term

        else if conclTerm.variables.contains(cTypeVar) then
          errors += s"Error: $ruleName conclusion uses a type variable bound to a term variable: $cTypeVar"

        else if r.premises.isEmpty && concl.env.parts.isEmpty then
          errors += s"Error: $ruleName conclusion uses a type variable but has no premises or environment: $cTypeVar"
        
        else if !r.premises.exists(p => p.bindsTypeVariableInTerm(cTypeVar)) then
          if concl.bindsTypeVariableInEnv(cTypeVar) then
            () // ok, bound in concl env
          else if !r.premises.exists(p => p.bindsTypeVariableInEnv(cTypeVar)) then
            errors += s"Error: $ruleName conclusion uses an unbound type variable: $cTypeVar"
          else
            errors += s"Error: $ruleName conclusion uses a type variable that is only bound in a premise environment: $cTypeVar"

    return errors.toSeq

  def validateAmbiguity(tsDecl: TypeSystemDecl): Seq[String] =
    val errors = ListBuffer.empty[String]

    for (dupRuleName, rs) <- tsDecl.rules.groupBy(_.name).filter(_._2.length > 1) do
      if dupRuleName.isDefined then
        errors += s"Error: duplicate rule name '${dupRuleName.get}'"

    for case Seq(r1, r2) <- tsDecl.rules.combinations(2) do
      val Judgement(_, HasType(e1, t1)) = r1.conclusion: @unchecked
      val Judgement(_, HasType(e2, t2)) = r2.conclusion: @unchecked
      if t1 != t2 && e1.overlaps(e2) then
        // As a simplification, only perform the validation if there are no premises
        // TODO: replace this by a validation if the premises could ever hold true for the same term
        if r1.premises.isEmpty || r2.premises.isEmpty then
          val r1Name = getRuleDisplayName(r1, tsDecl)
          val r2Name = getRuleDisplayName(r2, tsDecl)
          errors += s"Error: conclusions of $r1Name and $r2Name overlap but result in different types"
    
    return errors.toSeq

  def validateStructure(tsDecl: TypeSystemDecl): Seq[String] =
    val errors = ListBuffer.empty[String]

    for
      r <- tsDecl.rules
      case jr: JudgementRange <- r.premises
    do
      val ruleName = getRuleDisplayName(r, tsDecl)
      errors ++= validateStructure(ruleName, jr)

    return errors.toSeq

  private def validateStructure(ruleName: String, range: JudgementRange): Seq[String] =
    val errors = ListBuffer.empty[String]

    val JudgementRange(
      from @ Judgement(fromEnv, HasType(fromTerm, fromTyp)),
      to @ Judgement(toEnv, HasType(toTerm, toType))
    ) = range: @unchecked
    
    (fromTerm, toTerm) match {
      case (Term.Variable(fromVar), Term.Variable(toVar)) => (extractIndex(fromVar), extractIndex(toVar)) match {
        case (Some((fromIdent, fromIdxStr)), Some((toIdent, toIdxStr))) =>
          if fromIdent != toIdent then
            errors += f"Error: $ruleName premise range start and end must type the same variable, module index"

          try
            val fromIdx = fromIdxStr.toInt
            val toIdx = toIdxStr.toIntOption.getOrElse(Int.MaxValue)
            if fromIdx > toIdx then
              errors += f"Error: $ruleName premise range start index must be less than or equal to end index"
          catch case _: NumberFormatException =>
            errors += f"Error: $ruleName premise range start must be indexed by an integer: $fromVar"

          val idxPlaceholder = "$IDX"
          if from.replaceIndex(fromIdxStr, idxPlaceholder) != to.replaceIndex(toIdxStr, idxPlaceholder) then
            errors += f"Error: $ruleName premise range start and end must be the same modulo their respective main index"
        case (_, _) =>
          errors += f"Error: $ruleName premise range start and end must type a variable with an index, but at least one doesn't: $fromVar, $toVar"
      } 
      case _ =>
        errors += f"Error: $ruleName premise range start and end must type a variable, but at least one doesn't: $fromTerm, $toTerm"
    }

    return errors.toSeq