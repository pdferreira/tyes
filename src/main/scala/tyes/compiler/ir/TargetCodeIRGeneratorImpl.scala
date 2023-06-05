package tyes.compiler.ir

import TargetCodeNodeOperations.*
import tyes.compiler.RuntimeAPIGenerator
import utils.collections.*

private val TCFC = TargetCodeForCursor

class TargetCodeIRGeneratorImpl extends TargetCodeIRGenerator[TargetCodeNode](TargetCodeNodeOperations):

  def generate(irNode: IRNode[TargetCodeNode]): TargetCodeNode = generate(irNode, eitherIsExpected = true)
    
  private def generate(irNode: IRNode[TargetCodeNode], eitherIsExpected: Boolean): TargetCodeNode = irNode match {
    case IRNode.Unexpected => 
      TargetCodeNode.Throw(TargetCodeTypeRef("Exception"), TargetCodeNode.Text("unexpected"))
    
    case IRNode.Error(err) =>
      assert(eitherIsExpected, "Returning Either must be expected in Error")
      RuntimeAPIGenerator.genError(err)
    
    case IRNode.Result(res, canFail) =>
      assert(eitherIsExpected || !canFail, s"Returning Either must be expected in Result ($eitherIsExpected) or it can't fail (${!canFail})") 
      if eitherIsExpected && !canFail 
      then wrapAsRight(res) 
      else res
    
    case IRNode.And(IRInstr.Decl(resVar, exp) :: Nil, IRNode.Result(TargetCodeNode.Var(resVar2), /*canFail*/false)) 
      if resVar == resVar2 && canFail(exp) 
    =>
      generate(exp, eitherIsExpected)
    
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(TargetCodeNode.Var(resVar2), resCanFail))
      if resVar == resVar2 && canFail(exp) == resCanFail 
    =>
      generate(IRNode.And(cs, exp), eitherIsExpected)
    
    case IRNode.And(conds, next) =>
      assert(eitherIsExpected, "Returning Either must be expected in Ands")
      TargetCodeNode.For(
        cursors = inDataFlowOrder(conds.map(generate)), 
        body = generate(next, eitherIsExpected = false)
      )
    
    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = generate(otherwise, eitherIsExpected || failureIsPossible)

      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TargetCodeNode.If(
          cond,
          generate(next, eitherIsExpected || failureIsPossible),
          elseNode
        )
      }
    
    case IRNode.Or(main, _) if !canFail(main) => generate(main, eitherIsExpected)
    
    case IRNode.Or(main, alt) =>
      val mainNode = generate(main, eitherIsExpected)
      val altNode = generate(alt, eitherIsExpected = true)
      TargetCodeNode.Apply(
        TargetCodeNode.Field(
          mainNode,
          "orElse"
        ),
        altNode
      )
  }

  def generate(irInstr: IRInstr[TargetCodeNode]): TargetCodeForCursor = irInstr match {
    case IRInstr.Cond(cond, err) => 
      TargetCodeForCursor.Iterate("_", RuntimeAPIGenerator.genCheck(cond, err))
    case IRInstr.Decl(resVar, exp) =>
      if canFail(exp) then 
        TargetCodeForCursor.Iterate(resVar, generate(exp, eitherIsExpected = true))
      else
        TargetCodeForCursor.Let(resVar, generate(exp, eitherIsExpected = false))
  }

  private def wrapAsRight(value: TargetCodeNode): TargetCodeNode = TargetCodeNode.Apply(TargetCodeNode.Var("Right"), value)

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

    val undeclaredNames = collection.mutable.Set.from(cursors.map(getDeclaredName).flatten)
    val res = collection.mutable.ListBuffer.from(cursors)
    var i = 0
    while i < res.length do
      val c = res(i)
      val missingDeps = getDependencies(c).intersect(undeclaredNames)
      if missingDeps.isEmpty then
        i += 1
        getDeclaredName(c).map { name => 
          undeclaredNames -= name
        }
      else if i + 1 < res.length then
        val next = res(i + 1)
        res(i) = next
        res(i + 1) = c

    res.toSeq
