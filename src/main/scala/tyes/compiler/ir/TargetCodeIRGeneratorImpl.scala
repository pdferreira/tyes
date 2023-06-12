package tyes.compiler.ir

import tyes.compiler.target.TargetCodeForCursor
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeNodeOperations.*
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.RuntimeAPIGenerator
import utils.collections.*

private val TCFC = TargetCodeForCursor
private val TCN = TargetCodeNode

class TargetCodeIRGeneratorImpl extends TargetCodeIRGenerator:

  def generate(irNode: IRNode): TargetCodeNode = generate(irNode, eitherIsExpected = true)
    
  private def generate(irNode: IRNode, eitherIsExpected: Boolean): TargetCodeNode = irNode match {
    case IRNode.Unexpected => 
      TCN.Throw(TargetCodeTypeRef("Exception"), TCN.Text("unexpected"))
    
    case IRNode.Error(err) =>
      assert(eitherIsExpected, "Returning Either must be expected in Error")
      RuntimeAPIGenerator.genError(err)
    
    case IRNode.Result(res, canFail) =>
      assert(eitherIsExpected || !canFail, s"Returning Either must be expected in Result ($eitherIsExpected) or it can't fail (${!canFail})") 
      if eitherIsExpected && !canFail 
      then wrapAsRight(res) 
      else res
    
    case IRNode.And(IRInstr.Check(exp, Some(resVar)) :: Nil, IRNode.Result(TCN.Var(resVar2), /*canFail*/false)) 
      if resVar == resVar2 && canFail(exp) 
    =>
      generate(exp, eitherIsExpected)
    
    case IRNode.And(cs :+ IRInstr.Check(exp, Some(resVar)), IRNode.Result(TCN.Var(resVar2), resCanFail))
      if resVar == resVar2 && canFail(exp) == resCanFail 
    =>
      generate(IRNode.And(cs, exp), eitherIsExpected)
    
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

  def generate(irInstr: IRInstr): TargetCodeForCursor = irInstr match {
    case IRInstr.Cond(cond, err) => 
      TargetCodeForCursor.Iterate("_", RuntimeAPIGenerator.genCheck(cond, err))

    case IRInstr.Check(exp, resVarOpt) =>
      val resVar = resVarOpt.getOrElse("_")
      if canFail(exp) then 
        TargetCodeForCursor.Iterate(resVar, generate(exp, eitherIsExpected = true))
      else
        TargetCodeForCursor.Let(resVar, generate(exp, eitherIsExpected = false))
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
    def getDeclaredName(cursor: TargetCodeForCursor): Option[String] = cursor match {
      case TCFC.Filter(_) => None
      case TCFC.Iterate(name, _) => Some(name)
      case TCFC.Let(name, _) => Some(name)
    }

    def getDependencies(cursor: TargetCodeForCursor): Set[String] = cursor match {
      case TCFC.Filter(exp) => freeNames(exp).toSet
      case TCFC.Iterate(_, col) => freeNames(col).toSet
      case TCFC.Let(_, exp) => freeNames(exp).toSet 
    }

    // Build a map of all the undeclared dependencies for each cursor
    // but only considering the names that are actually declared within this `for`
    // otherwise this is pointless because there'll always be `exp` and other free names
    val undeclaredNames = Set.from(cursors.map(getDeclaredName).flatten)
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
        name <- getDeclaredName(c)
        deps <- missingDepsByCursor.values
      do
        deps.remove(name)

      res += c

    // Finally pack the collected cursors
    res.toSeq
