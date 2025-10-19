package tyes.compiler

import scala.collection.mutable
import tyes.compiler.target.TargetCodeDecl
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.*
import tyes.model.*
import tyes.model.indexes.*
import tyes.model.terms.TermOps
import tyes.model.terms.TermRange
import tyes.model.terms.TermVariable
import tyes.model.TyesLanguageExtensions.*

class RangeIRGenerator(
  private val typeIRGenerator: TypeIRGenerator,
  private val expClassTypeRef: TCTypeRef,
):

  private val expClassParametrizedTypeRef = expClassTypeRef.copy(params = Seq(typeIRGenerator.typeEnumTypeRef))

  private def getExtractorName[TTerm](range: TermRange[TTerm]) = s"${range.function}Range"

  private def getCollectionVarName(elemVarName: String): String = elemVarName + "s"

  private def getCollectionVarName[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): Option[String] =
    val vs = range.iteratedVariables
    assert(vs.size <= 1)
    vs.headOption.map(getCollectionVarName)

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

  /**
    * Generates a code pattern for an unlimited range (maxIndex is a Index.Var).
    * 
    * @return the pattern and a mapping from collection variables (e.g. `es`) to their element variables (e.g. `e`), if any 
    */
  def generateUnlimitedPattern(range: TermRange[Term], generatePat: Term => (TCP, Map[String, String])): (TCP, Map[String, String]) =
    if !range.argTemplates.forall({
      case Term.Variable(_) => true
      case Term.Type(Type.Variable(_)) => true
      case _ => false
    }) then
      throw new NotImplementedError(f"for $range")

    val colToElemVar = mutable.Map[String, String]()
    val (args, otherColVars) = range.argTemplates
      .map({
        case v @ Term.Variable(name) => extractIndex(name) match {
          case Some((name, idxStr)) if idxStr == range.cursor =>
            val colVarName = getCollectionVarName(name)
            colToElemVar(colVarName) = name
            Term.Variable(colVarName)
          case _ => v
        }
        case t @ Term.Type(Type.Variable(name)) => extractIndex(name) match {
          case Some((name, idxStr)) if idxStr == range.cursor =>
            val colVarName = getCollectionVarName(name)
            colToElemVar(colVarName) = name
            Term.Type(Type.Variable(colVarName))
          case _ => t
        }
        case t => throw new NotImplementedError(f"for $t")
      })
      .patch(from = range.holeArgIdx, other = range.holeSeed.toSeq, replaced = 0)
      .map(generatePat)
      .unzip

    (TCP.Extract(getExtractorName(range), args*), otherColVars.fold(colToElemVar.toMap)(_ ++ _))

  def generateConstructor[TTerm <: TermOps[TTerm, Any]](range: TermRange[TTerm]): TCN =
    RuntimeAPIGenerator.genFoldLeft1(
      TCN.Var(getCollectionVarName(range).get), 
      TCN.Field(TCN.Var(range.function), "apply")
    )