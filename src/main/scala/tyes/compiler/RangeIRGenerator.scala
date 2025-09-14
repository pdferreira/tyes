package tyes.compiler

import tyes.compiler.target.TargetCodeDecl
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeTypeRef
import tyes.model.*
import tyes.model.terms.TermOps
import tyes.model.terms.TermRange
import tyes.model.TyesLanguageExtensions.*

private type TCTypeRef = TargetCodeTypeRef
private type TCN = TargetCodeNode
private type TCD = TargetCodeDecl
private val TCN = TargetCodeNode
private val TCD = TargetCodeDecl

class RangeIRGenerator(
  private val typeIRGenerator: TypeIRGenerator,
  private val expClassTypeRef: TCTypeRef,
):

  private val expClassParametrizedTypeRef = expClassTypeRef.copy(params = Seq(typeIRGenerator.typeEnumTypeRef))

  private def getExtractorName[TTerm](range: TermRange[TTerm]) = s"${range.function}Range"

  private def getCollectionVarName[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): Option[String] =
    val vs = range.iteratedVariables
    assert(vs.size <= 1)
    vs.headOption.map(_ + "s")

  def generateExtractor[TTerm](range: TermRange[TTerm]): TCD =
    val paramVar = TCN.Var("exp"): TCN.Var
    val matchVar = TCN.Var("e"): TCN.Var
    val matchArgNames = Seq("l", "r")
    TCD.Extractor(
      getExtractorName(range),
      param = (paramVar.name -> expClassParametrizedTypeRef),
      retTypeRef = TCTypeRef("Seq", expClassParametrizedTypeRef),
      body = (_, _) => RuntimeAPIGenerator.genExtractRange(
        paramVar,
        TCN.Lambda(matchVar.name, TCN.Match(
          matchVar,
          branches = Seq(
            TCP.ADTConstructor(TCTypeRef(range.function), matchArgNames.map(TCP.Var(_))*) -> TCN.ADTConstructorCall(
              TCTypeRef("Tuple2"),
              matchArgNames.map(TCN.Var(_))*
            )
          )
        ))
      )
    )

  def generateUnboundPattern[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): Term =
    val elemsVarName = getCollectionVarName(range).get
    Term.Function(getExtractorName(range), Term.Variable(elemsVarName))

  def generateConstructor[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): TCN =
    RuntimeAPIGenerator.genFoldLeft1(
      TCN.Var(getCollectionVarName(range).get), 
      TCN.Field(TCN.Var(range.function), "apply")
    )