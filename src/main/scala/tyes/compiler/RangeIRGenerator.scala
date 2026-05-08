package tyes.compiler

import scala.collection.mutable
import tyes.compiler.target.TargetCodeDecl
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.*
import tyes.model.*
import tyes.model.indexes.*
import tyes.model.terms.*
import tyes.model.TyesLanguageExtensions.*

class RangeIRGenerator:

  private def getExtractorName[TTerm](range: TermRange[TTerm]) = s"${range.function}Range"

  private def getCollectionVarName(elemVarName: String): String = elemVarName + "s"

  private def getCollectionVarName[TTerm <: TermOps[TTerm, TConstant], TConstant](range: TermRange[TTerm]): Option[String] =
    val vs = range.iteratedVariables
    assert(vs.size <= 1)
    vs.headOption.map(getCollectionVarName)

  def generateExtractor[TTerm](
    range: TermRange[TTerm],
    paramVar: TCN.Var,
    paramType: TCTypeRef,
    genExtractedTypeRef: String => TCTypeRef
  ): TCD.Extractor =
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

    val extractRangeCode = genExtractRangeFn(paramVar, genExtractedTypeRef(range.function))
    TCD.Extractor(
      getExtractorName(range),
      param = (paramVar.name -> paramType),
      retTypeRef = None,
      body = (_, _) => extractRangeCode
    )

  /**
    * Generates a code pattern for an unlimited range (maxIndex is a Index.Var).
    * 
    * @return the pattern
    */
  def generateUnlimitedPattern[TTerm](
    range: TermRange[TTerm],
    generatePat: (TTerm, TargetCodeEnv, Map[String, String]) => TCP,
    codeEnv: TargetCodeEnv
  ): TCP =
    val colToElemVar = mutable.Map[String, String]()
    
    def collectColVar[T](v: T): T =
      val (colV, colVarEntry) = collectCollectionVar(v, range.cursor)
      colVarEntry.foreach((colVar, elemVar) => colToElemVar(colVar.name) = elemVar)
      colV

    val args = range.argTemplates
      .map(collectColVar)
      .patch(from = range.holeArgIdx, other = range.holeSeed.toSeq, replaced = 0)
      .map(p => generatePat(p, codeEnv, colToElemVar.toMap))

    TCP.Extract(getExtractorName(range), args*)

  def collectCollectionVar[T](v: T, cursor: String): (T, Option[(TermVariable[?], String)]) = v match {
    case v: TermVariable[T] => extractIndex(v.name) match {
      case Some((name, idxStr)) if idxStr == cursor =>
        val colVar = v.copy(name = getCollectionVarName(name))
        (colVar, Some(colVar -> name))
      case _ =>
        (v, None)
    }
    case t: SpecializedTerm[T, _, _] =>
      val (specialized, colVarEntry) = collectCollectionVar(t.specialized, cursor)
      (t.copy(specialized = specialized), colVarEntry)
        
    case t => throw new NotImplementedError(f"for $t")
  }

  def generateConstructor[TTerm <: TermOps[TTerm, TConstant], TConstant](
    range: TermRange[TTerm]
  )(
    genFunctionCode: String => TCN
  ): TCN =
    if range.argTemplates.size != 1 then
      throw new IllegalArgumentException("Not supported range with non-binary function")

    if range.holeSeed.isDefined then
      throw new IllegalArgumentException("Not supported range with seed")
    
    val genFold1 =
      if range.holeArgIdx == 0 then
        RuntimeAPIGenerator.genFoldLeft1
      else
        RuntimeAPIGenerator.genFoldRight1
    
    genFold1(TCN.Var(getCollectionVarName(range).get), genFunctionCode(range.function))
