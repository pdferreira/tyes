package tyes.compiler.target

import TargetCodeNodeOperations.*

private val TCN = TargetCodeNode
private val TCFC = TargetCodeForCursor
private val TCP = TargetCodePattern

/**
  * Transform a TargetCode* that isn't directly compilable to Scala code
  * in an equivalent TargetCode* that is.
  * 
  * Should be ran directly prior to invoking ScalaTargetCodeGenerator in 
  * order to ensure its transformations are not undone by a subsequent pass.
  */
class ScalaTargetCodeAdapter:
  
  def adapt(tcUnit: TargetCodeUnit): TargetCodeUnit =
    tcUnit.copy(decls = tcUnit.decls.map(adapt))

  def adapt(tcDecl: TargetCodeDecl): TargetCodeDecl = applyToChildren(tcDecl, adapt)

  def adapt(tcNode: TargetCodeNode): TargetCodeNode = applyUntil(tcNode, {
    // Scala doesn't support cursor-lets as the first in a for-comprehension
    // so instead we transform those as let + for-comprehension. This rule is
    // already present in the simplifier but for a volatile reason (code style)
    // so we keep it here as well.
    case TCN.For(TCFC.Let(pat, exp) :: cs, bodyExp) =>
      adapt(TCN.Let(pat, exp, TCN.For(cs, bodyExp)))

    // Scala doesn't always support destructuring of a composite variable on
    // an iterator. It only works properly in monadic data types, which is
    // not the case of Either[A, B] which has no sound withFilter implementation.
    // Instead we split the cursor in an iterate + let, pattern matching on the let.
    case TCN.For(TCFC.Iterate(pat, col) :: cs, bodyExp) if isComposite(pat) =>
      val tmpPatName = "_" + getVarNameFromCompositePattern(pat)
      if freeNames(tcNode).contains(tmpPatName) then
        throw new NotImplementedError("Nameclashing not implemented for temporary variable")
      
      adapt(TCN.For(
        TCFC.Iterate(TCP.Var(tmpPatName), col) +: TCFC.Let(pat, TCN.Var(tmpPatName)) +: cs,
        bodyExp
      ))
  })

  private def getVarNameFromCompositePattern(tcp: TargetCodePattern): String = tcp match {
    case TCP.ADTConstructor(typeRef, _*) => typeRef.name.filter(_.isUpper).toLowerCase
    case TargetCodePattern.WithType(_, _) => ???
    case _ =>
      assert(isComposite(tcp), s"Expected pattern to be composite, but instead got: $tcp")
      throw new NotImplementedError(tcp.toString)
  }
