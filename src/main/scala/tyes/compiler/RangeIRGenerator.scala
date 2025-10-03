package tyes.compiler

import tyes.compiler.target.TargetCodeDecl
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.*
import tyes.model.*
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
    val matchArgNames = Seq("l", "r")

    def genExtractArgsCode(argsContainer: Seq[TCN] => TCN): TCN =
      TCN.Lambda(matchVar.name, TCN.Match(
        matchVar,
        branches = Seq(
          TCP.ADTConstructor(TCTypeRef(range.function), matchArgNames.map(TCP.Var(_))*) -> argsContainer(matchArgNames.map(TCN.Var(_)))
        )
      ))

    val argsSize = range.argTemplates.size + 1
    val genExtractRangeFn =
      if argsSize == 2 && range.holeArgIdx == 0 && range.holeSeed.isEmpty then
        RuntimeAPIGenerator.genExtractRangeL
      else if argsSize == 2 && range.holeArgIdx == 1 && range.holeSeed.isEmpty then
        RuntimeAPIGenerator.genExtractRangeR
      else if range.holeArgIdx > 0 && range.holeSeed.isDefined then
        RuntimeAPIGenerator.genExtractRange(_, _, range.holeArgIdx, _)
      else
        throw new NotImplementedError(f"for ${range}")

    val (extractRangeTypeRef, extractRangeCode) = genExtractRangeFn(paramVar, expClassParametrizedTypeRef, genExtractArgsCode)
    TCD.Extractor(
      getExtractorName(range),
      param = (paramVar.name -> expClassParametrizedTypeRef),
      retTypeRef = extractRangeTypeRef,
      body = (_, _) => extractRangeCode
    )

  def generateUnboundPattern(range: TermRange[Term]): Term =
    val elemsVarName = getCollectionVarName(range).get
    val elemsVar = Term.Variable(elemsVarName)
    val args = Seq(elemsVar) ++ range.holeSeed.toSeq
    Term.Function(getExtractorName(range), args*)

  def generateConstructor[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): TCN =
    RuntimeAPIGenerator.genFoldLeft1(
      TCN.Var(getCollectionVarName(range).get), 
      TCN.Field(TCN.Var(range.function), "apply")
    )