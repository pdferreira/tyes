package tyes.compiler.ir.experiments

import tyes.compiler.ir.*
import tyes.compiler.target.*
import tyes.compiler.target.TargetCodeNodeOperations.*

private val TCN = TargetCodeNode
private val TCTypeRef = TargetCodeTypeRef

class ExceptionBasedTargetCodeIRGenerator extends TargetCodeIRGenerator:

  def generate(irNode: IRNode): TargetCodeNode = irNode match {
    case IRNode.Unexpected => TCN.Throw(TCTypeRef("Exception"), TCN.Text("unexpected"))
    case IRNode.Error(IRError.Generic(err)) => TCN.Throw(TCTypeRef("TypeError"), err)
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Check(exp, TCP.Var(resVar)), IRNode.Result(TCN.Var(resVar2), resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      generate(IRNode.And(cs, exp))
    case IRNode.And(conds, next) =>
      conds.map(generate).foldRight(generate(next)) { case (compose, nextNode) => compose(nextNode) }
    case IRNode.Switch(branches, otherwise) =>
      val otherwiseNode = generate(otherwise)
      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TCN.If(cond, generate(next), elseNode)
      }
    case IRNode.Or(main, _) if !canFail(main) => generate(main)
    case IRNode.Or(main, alt) =>
      val mainNode = generate(main)
      val altNode = generate(alt)
      TCN.Try(mainNode, TCTypeRef("TypeError"), altNode)
  }

  def generate(irInstr: IRInstr): TargetCodeNode => TargetCodeNode = irInstr match {
    case IRInstr.Cond(cond, IRError.Generic(err)) =>
      nextNode => TCN.If(negate(cond), TCN.Throw(TCTypeRef("TypeError"), err), nextNode)
    case IRInstr.Check(exp, resPat) =>
      nextNode => TCN.Let(resPat, generate(exp), nextNode)
  }