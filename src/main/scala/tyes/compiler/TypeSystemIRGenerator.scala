package tyes.compiler

import tyes.compiler.Orderings.given
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRError
import tyes.compiler.ir.TargetCodeIRGenerator
import tyes.compiler.target.TargetCodeDecl
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.target.TargetCodeUnit
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*
import utils.StringExtensions.*
import utils.collections.*

private val TCD = TargetCodeDecl
private val TCN = TargetCodeNode
private val TCP = TargetCodePattern
private val TCTypeRef = TargetCodeTypeRef

class TypeSystemIRGenerator(
  private val commonEnvName: String,
  private val targetCodeIRGenerator: TargetCodeIRGenerator
):

  private val expClassTypeRef = TCTypeRef("LExpression")
  private val expVar: TCN.Var = TCN.Var("exp")

  private val typeIRGenerator = new TypeIRGenerator()
  private val termIRGenerator = new TermIRGenerator(typeIRGenerator)
  private val envIRGenerator = new EnvironmentIRGenerator(typeIRGenerator, commonEnvName)
  private val ruleIRGenerator = new RuleIRGenerator(typeIRGenerator, termIRGenerator, envIRGenerator, expVar)

  def generate(tsDecl: TypeSystemDecl): TargetCodeUnit =
    val className = s"${tsDecl.name.getOrElse("")}TypeSystem"
    val typeEnumTypeRef = typeIRGenerator.typeEnumTypeRef

    TargetCodeUnit(className, Seq(
      TCD.Import(Seq("tyes", "runtime"), all = true),
      TCD.Import(Seq("example"), all = true),
      TCD.Class(
        className,
        inherits = Seq(
          TCTypeRef("TypeSystem", expClassTypeRef)
        ),
        decls = Seq(
          TCD.Type("T", typeEnumTypeRef),
          typeIRGenerator.generateDecl(tsDecl),
          TCD.Method(
            "typecheck",
            params = Seq(
              expVar.name -> expClassTypeRef.copy(params = Seq(typeEnumTypeRef)),
              envIRGenerator.generateParameter()
            ),
            retTypeRef = TCTypeRef("Either", TCTypeRef("String"), typeEnumTypeRef),
            body = generateTypecheckBody(expVar, tsDecl.rules)
          )
        )
      )
    ))

  private def generateTypecheckBody(expVar: TCN.Var, rules: Seq[RuleDecl]): TargetCodeNode =
    val defaultCase = TCP.Any -> RuntimeAPIGenerator.genError(IRError.NoType(expVar))

    val ruleCases = rules
      .groupBy(r => ruleIRGenerator.getTemplate(r))
      .toSeq
      .sortBy((rTemplate, _) => rTemplate: Term)
      .map((rTemplate, rs) => generateTypecheckCase(rTemplate, rs))

    TCN.Match(
      expVar,
      branches = ruleCases :+ defaultCase
    )

  private def generateTypecheckCase(rTemplate: Term, rules: Seq[RuleDecl]): (TargetCodePattern, TargetCodeNode) =
    val codeEnv = TargetCodeEnv()
    for v <- rTemplate.termVariables do
      codeEnv.requestIdentifier(v)

    val rTemplateCode = termIRGenerator.generatePattern(rTemplate)
    val rImplIntermediateCode = groupNonOverlappingRules(rules)
      // For each of the groups:
      // - if it has a single rule, use its resulting node
      // - otherwise if all have pre-conditions, create a switch node
      .map(rs => 
        val rIRs = rs.map(r => ruleIRGenerator.generate(r, codeEnv, rTemplate))
        if rIRs.exists(_.condition.isEmpty) then
          val rulesDesc = rs.zipWithIndex.map((r, idx) => r.name.getOrElse(idx.toString)).mkString(" and ")
          assert(rIRs.length == 1, s"If there're no conditions, expected only a single rule in the group, not $rulesDesc")
          rIRs.head.node
        else
          IRNode.Switch(
            branches = rIRs.map(ir => ir.condition.get -> ir.node),
            otherwise = IRNode.Error(IRError.NoType(expVar))
          )
      )
      // Then unite the resulting node of each group using Or nodes
      // to reflect the fact that they overlap
      .foldLeft1(IRNode.Or.apply)
    
    val rImplTargetCode = targetCodeIRGenerator.generate(rImplIntermediateCode)
    rTemplateCode -> rImplTargetCode

  private def getConclusionTerm(rule: RuleDecl): Term = rule.conclusion.assertion match {
    case HasType(term, _) => term
  }

  private def groupNonOverlappingRules(rules: Seq[RuleDecl]): Seq[Seq[RuleDecl]] = rules match {
    case Nil => Seq()
    case r :: rs =>
      val rTerm = getConclusionTerm(r)
      val norGroups = groupNonOverlappingRules(rs)

      // Find the first group whose rules don't overlap with `r`
      val norGroupIdx = norGroups.indexWhere(nors => 
        nors
          .map(getConclusionTerm)
          .forall(orTerm => !rTerm.overlaps(orTerm))
      )

      // If none is found, create a new group with `r`, otherwise add it to the one found
      if norGroupIdx == -1 then
        Seq(r) +: norGroups
      else
        val (before, idxGroup +: after) = norGroups.splitAt(norGroupIdx)
        before ++ ((r +: idxGroup) +: after)
  }
