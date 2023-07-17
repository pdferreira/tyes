package tyes.compiler.ir

import tyes.compiler.target.TargetCodeForCursor
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeNodeOperations.*
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.RuntimeAPIGenerator
import utils.collections.*

private val TCFC = TargetCodeForCursor
private val TCN = TargetCodeNode
private val TCP = TargetCodePattern
private val TCTypeRef = TargetCodeTypeRef

class TargetCodeIRGeneratorImpl(
  private val expVar: TCN.Var
) extends TargetCodeIRGenerator:

  def generate(irNode: IRNode): TargetCodeNode = generate(irNode, eitherIsExpected = true)
    
  private def generate(irNode: IRNode, eitherIsExpected: Boolean): TargetCodeNode = irNode match {
    case IRNode.Unexpected => 
      TCN.Throw(TargetCodeTypeRef("Exception"), TCN.Text("unexpected"))
    
    case IRNode.Error(err) =>
      assert(eitherIsExpected, "Returning Either must be expected in Error")
      RuntimeAPIGenerator.genError(err)

    case IRNode.Type(irType) => generate(irType, eitherIsExpected)
    
    case IRNode.And(IRCond.TypeDecl(TCP.Var(resVar), typExp, None) :: Nil, IRNode.Type(IRType.FromCode(TCN.Var(resVar2), /*isOptional*/false))) 
      if resVar == resVar2 && canFail(typExp)  
    =>
      generate(typExp, eitherIsExpected)
    
    case IRNode.And(cs :+ IRCond.TypeDecl(TCP.Var(resVar), typExp, None), IRNode.Type(IRType.FromCode(TCN.Var(resVar2), isOptional)))
      if resVar == resVar2 && canFail(typExp) == isOptional
    =>
      generate(IRNode.And(cs, typExp), eitherIsExpected)
    
    case IRNode.And(conds, next) =>
      assert(eitherIsExpected, "Returning Either must be expected in Ands")
      TCN.For(
        cursors = inDataFlowOrder(conds.map(generate)), 
        body = generate(next, eitherIsExpected = false)
      )
    
    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = generate(otherwise, eitherIsExpected || failureIsPossible)

      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TCN.If(
          cond,
          generate(next, eitherIsExpected || failureIsPossible),
          elseNode
        )
      }
    
    case IRNode.Or(main, _) if !canFail(main) => generate(main, eitherIsExpected)
    
    case IRNode.Or(main, alt) =>
      val mainNode = generate(main, eitherIsExpected)
      val altNode = generate(alt, eitherIsExpected = true)
      TCN.Apply(
        TCN.Field(
          mainNode,
          "orElse"
        ),
        altNode
      )
  }

  private def generate(irType: IRType, eitherIsExpected: Boolean): TargetCodeNode = irType match {
    case IRType.FromCode(typCode, /*isOptional*/true) =>
      if eitherIsExpected
      then RuntimeAPIGenerator.genCheckTypeDeclared(typCode, expVar)
      else ???

    case IRType.FromCode(typCode, /*isOptional*/false) =>
      if eitherIsExpected
      then wrapAsRight(typCode)
      else typCode

    case IRType.Induction(expCode, envCode) =>
      if eitherIsExpected
      then RuntimeAPIGenerator.genTypecheck(expCode, envCode)
      else ???

    case IRType.EnvGet(envVar, keyCode) =>
      if eitherIsExpected
      then RuntimeAPIGenerator.genEnvironmentGet(TCN.Var(envVar), keyCode)
      else ???
  }

  private def generate(irCond: IRCond): TargetCodeForCursor = irCond match {
    case IRCond.EnvSizeIs(envVar, size) => 
      TCFC.Iterate(TCP.Any, RuntimeAPIGenerator.genCheckEnvSize(TCN.Var(envVar), size))

    case IRCond.TypeEquals(t1Code, t2Code) =>
      TCFC.Iterate(
        TCP.Any,
        RuntimeAPIGenerator.genCheck(
          TCN.Equals(t1Code, t2Code),
          IRError.UnexpectedType(t1Code, t2Code)
        )
      )

    case IRCond.TypeDecl(declPat, typExp, None) if !canFail(typExp) =>
      TCFC.Let(declPat, generate(typExp, eitherIsExpected = false))
      
    case IRCond.TypeDecl(declPat, typExp, expectOpt) =>
      val typExpCode = generate(typExp, eitherIsExpected = true)
      TCFC.Iterate(
        declPat, 
        expectOpt match {
          case None => typExpCode
          case Some(irExpect) => genExpectationCheck(typExpCode, irExpect)
        }
      )

  }

  private def genExpectationCheck(
    typeProviderCode: TargetCodeNode,
    expectation: IRTypeExpect
  ): TargetCodeNode = expectation match {
    case IRTypeExpect.OfType(typeRef) =>
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeRef)
    
    case IRTypeExpect.EqualsTo(typeCode) =>
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeCode)
  }

  private def wrapAsRight(value: TargetCodeNode): TargetCodeNode = TCN.Apply(TCN.Var("Right"), value)

  /**
    * Ensures the collection order respects the data flow, taking into account
    * the order of declarations. Preserves the provided relative order between cursors
    * except in when it detects used-before-declared scenarios.
    *
    * @param cursors
    * @return Reordered collection of cursors
    */
  private def inDataFlowOrder(cursors: Seq[TargetCodeForCursor]): Seq[TargetCodeForCursor] =
    def getDeclaredNames(cursor: TargetCodeForCursor): Set[String] = cursor match {
      case TCFC.Filter(_) => Set()
      case TCFC.Iterate(pat, _) => boundNames(pat)
      case TCFC.Let(pat, _) => boundNames(pat)
    }

    def getDependencies(cursor: TargetCodeForCursor): Set[String] = cursor match {
      case TCFC.Filter(exp) => freeNames(exp).toSet
      case TCFC.Iterate(_, col) => freeNames(col).toSet
      case TCFC.Let(_, exp) => freeNames(exp).toSet 
    }

    // Build a map of all the undeclared dependencies for each cursor
    // but only considering the names that are actually declared within this `for`
    // otherwise this is pointless because there'll always be `exp` and other free names
    val undeclaredNames = Set.from(cursors.map(getDeclaredNames).flatten)
    val missingDepsByCursor = cursors
      .map { c =>
        val missingDeps = getDependencies(c).intersect(undeclaredNames)
        (c, collection.mutable.Set.from(missingDeps))
      }
      .toMap

    // Go through all the cursors, preferrably in order but picking those with
    // 0 missing dependencies first. For each pick, update the missing dependencies
    // and if there're none with 0 dependencies, fallback to picking the first one.
    val res = collection.mutable.ListBuffer[TargetCodeForCursor]()
    val cursorsToProcess = collection.mutable.Queue.from(cursors)
    while !cursorsToProcess.isEmpty do
      val c = cursorsToProcess
        .dequeueFirst(c => missingDepsByCursor(c).isEmpty)
        .getOrElse { cursorsToProcess.dequeue() }
      
      for 
        name <- getDeclaredNames(c)
        deps <- missingDepsByCursor.values
      do
        deps.remove(name)

      res += c

    // Finally pack the collected cursors
    res.toSeq
