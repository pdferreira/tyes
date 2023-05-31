package tyes.compiler

import tyes.compiler.Orderings.given
import tyes.compiler.ir.TargetCodeDecl
import tyes.compiler.ir.TargetCodeIRGenerator
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodeUnit
import tyes.compiler.ir.TargetCodeTypeRef
import tyes.model.*
import utils.StringExtensions.*

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
      .map((rTemplate, rs) => {
        val codeEnv = TargetCodeEnv()
        for v <- rTemplate.variables do
          codeEnv.registerIdentifier(v, TCN.Var(v))
        
        val rTemplateCode = termIRGenerator.generate(rTemplate)
        val rImplIntermediateCode = ruleIRGenerator.generate(rs.head, codeEnv, rTemplate)
        val rImplTargetCode = targetCodeIRGenerator.generate(rImplIntermediateCode)
        rTemplateCode -> rImplTargetCode 
      })

    TCN.Match(
      expVar,
      branches = ruleCases :+ defaultCase
    )
