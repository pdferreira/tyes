package tyes.compiler

import tyes.compiler.Orderings.given
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.TargetCodeDecl
import tyes.compiler.ir.TargetCodeIRGenerator
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodeUnit
import tyes.compiler.ir.TargetCodeTypeRef
import tyes.model.*
import utils.StringExtensions.*
import utils.collections.*

private val TCN = TargetCodeNode
private val TCD = TargetCodeDecl
private val TCTypeRef = TargetCodeTypeRef

class TypeSystemIRGenerator(
  private val commonEnvName: String,
  private val targetCodeIRGenerator: TargetCodeIRGenerator[TargetCodeNode]
):

  private val expClassTypeRef = TCTypeRef("LExpression")
  private val defaultEnvVarName = commonEnvName.decapitalize

  private val typeIRGenerator = new TypeIRGenerator()
  private val termIRGenerator = new TermIRGenerator(typeIRGenerator)
  private val ruleIRGenerator = new RuleIRGenerator(typeIRGenerator, termIRGenerator)

  def generate(tsDecl: TypeSystemDecl): TargetCodeUnit =
    val className = s"${tsDecl.name.getOrElse("")}TypeSystem"
    val expVar: TCN.Var = TCN.Var("exp")
    val typeEnumTypeRef = typeIRGenerator.typeEnumTypeRef

    TargetCodeUnit(className, Seq(
      TCD.Import(Seq("tyes", "runtime"), all = true),
      TCD.Import(Seq("example"), all = true),
      TCD.Class(
        className,
        inherits = Seq(
          TCTypeRef("TypeSystem", expClassTypeRef),
          TCTypeRef("TypeOperations")
        ),
        decls = Seq(
          TCD.Type("T", typeEnumTypeRef),
          typeIRGenerator.generateDecl(tsDecl),
          TCD.Method(
            "typecheck",
            params = Seq(
              expVar.name -> expClassTypeRef.copy(params = Seq(typeEnumTypeRef)),
              defaultEnvVarName -> TCTypeRef("Map", TCTypeRef("String"), typeEnumTypeRef)
            ),
            retTypeRef = TCTypeRef("Either", TCTypeRef("String"), typeEnumTypeRef),
            body = generateTypecheckBody(expVar, tsDecl.rules)
          )
        )
      )
    ))

  private def generateTypecheckBody(expVar: TCN.Var, rules: Seq[RuleDecl]): TargetCodeNode =
    val defaultCase = TCN.Var("_") -> TCN.Apply(
      TCN.Var("Left"), 
      TCN.FormattedText("TypeError: no type for `", expVar, "`")
    )

    val ruleCases = rules
      .groupBy(r => ruleIRGenerator.getTemplate(r))
      .toSeq
      .sortBy((rTemplate, _) => rTemplate: Term)
      .map((rTemplate, rs) => generateTypecheckCase(rTemplate, rs))

    TCN.Match(
      expVar,
      branches = ruleCases :+ defaultCase
    )

  private def generateTypecheckCase(rTemplate: Term, rules: Seq[RuleDecl]): (TargetCodeNode, TargetCodeNode) =
    val codeEnv = TargetCodeEnv()
    for v <- rTemplate.variables do
      codeEnv.registerIdentifier(v, TCN.Var(v))
    
    val rTemplateCode = termIRGenerator.generate(rTemplate)
    val rImplIntermediateCode = groupNonOverlappingRules(rules)
      // For each of the groups:
      // - if it has a single rule, use its resulting node
      // - otherwise, "concat" the Switch nodes resulting from each rule as by definition they don't overlap
      .map(rs => rs
        .map(r => ruleIRGenerator.generate(r, codeEnv, rTemplate))
        .foldRight1({
          case (IRNode.Switch(bs, IRNode.Error(_)), accNode) => IRNode.Switch(bs, accNode)
          case (IRNode.Switch(_, otherwise), _) => ???
          case (irNode, _) =>
            val rulesDesc = rs.zipWithIndex.map((r, idx) => r.name.getOrElse(idx.toString)).mkString(" and ")
            val irNodeTypeName = irNode.getClass.getSimpleName
            throw new Exception(s"If the rules $rulesDesc don't overlap, expected a Switch node instead of $irNodeTypeName")
        })
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
