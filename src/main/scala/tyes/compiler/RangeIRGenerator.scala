package tyes.compiler

import tyes.compiler.target.TargetCodeDecl
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.*
import tyes.model.*
import tyes.model.indexes.*
import tyes.model.terms.TermOps
import tyes.model.terms.TermRange
import tyes.model.TyesLanguageExtensions.*

object RangeIRGenerator:

  def getCollectionVarName(elemVarName: String): String = elemVarName + "s"


class RangeIRGenerator(
  private val typeIRGenerator: TypeIRGenerator,
  private val expClassTypeRef: TCTypeRef,
):

  private val expClassParametrizedTypeRef = expClassTypeRef.copy(params = Seq(typeIRGenerator.typeEnumTypeRef))

  private def getExtractorName[TTerm](range: TermRange[TTerm]) = s"${range.function}Range"

  private def getCollectionVarName[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): Option[String] =
    val vs = range.iteratedVariables
    assert(vs.size <= 1)
    vs.headOption.map(RangeIRGenerator.getCollectionVarName)

  def generateExtractor[TTerm](range: TermRange[TTerm]): TCD =
    val paramVar = TCN.Var("exp"): TCN.Var
    val matchVar = TCN.Var("e"): TCN.Var
    val argsSize = range.argTemplates.size + 1
    val genExtractRangeFn =
      if argsSize == 2 && range.holeArgIdx == 0 && range.holeSeed.isEmpty then
        RuntimeAPIGenerator.genExtractRangeLNoSeed
      else if argsSize == 2 && range.holeArgIdx == 1 && range.holeSeed.isEmpty then
        RuntimeAPIGenerator.genExtractRangeRNoSeed
      else if range.holeArgIdx > 0 && range.holeSeed.isDefined then
        RuntimeAPIGenerator.genExtractRangeR
      else
        throw new NotImplementedError(f"for ${range}")

    val extractRangeCode = genExtractRangeFn(paramVar, TCTypeRef(range.function, typeIRGenerator.typeEnumTypeRef))
    TCD.Extractor(
      getExtractorName(range),
      param = (paramVar.name -> expClassParametrizedTypeRef),
      retTypeRef = None,
      body = (_, _) => extractRangeCode
    )

  def generateUnboundPattern(range: TermRange[Term], generatePat: Term => TCP): TCP =
    val args = range.argTemplates
      .patch(from = range.holeArgIdx, other = range.holeSeed.toSeq, replaced = 0)
      .map(generatePat)

    TCP.Extract(getExtractorName(range), args*)

  def generateConstructor[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): TCN =
    RuntimeAPIGenerator.genFoldLeft1(
      TCN.Var(getCollectionVarName(range).get), 
      TCN.Field(TCN.Var(range.function), "apply")
    )