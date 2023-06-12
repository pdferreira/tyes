package tyes.compiler.ir

private type TCN = TargetCodeNode
private val TCN = TargetCodeNode
private val TCP = TargetCodePattern
private val TCTypeRef = TargetCodeTypeRef

class IfBasedTargetCodeIRGenerator extends TargetCodeIRGenerator(TargetCodeNodeOperations):

  def generate(irNode: IRNode): TCN = generate(irNode, failureIsPossible = false)

  // TODO: find better name for failureIsPossible param and canFail field
  def generate(irNode: IRNode, failureIsPossible: Boolean): TCN = irNode match {
    case IRNode.Unexpected => TCN.Throw(TCTypeRef("Exception"), TCN.Text("unexpected"))
    case IRNode.Error(IRError.Generic(err)) => wrapAsLeft(err)
    case IRNode.Result(res, canFail) => if failureIsPossible && !canFail then wrapAsRight(res) else res
    case IRNode.And(cs :+ IRInstr.Check(exp, Some(resVar)), IRNode.Result(TCN.Var(resVar2), resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      generate(IRNode.And(cs, exp), failureIsPossible || canFail(exp) != resCanFail)
    case IRNode.And(IRInstr.Check(exp, resVar) +: cs, next) =>
      val letExp = 
        if canFail(exp) then
          TCN.Match(generate(exp), Seq(
            TCP.ADTConstructor(TCTypeRef("Right"), TCP.Var("v")) -> TCN.Var("v"),
            TCP.Var("left") -> TCN.Return(TCN.Var("left"))
          ))
        else
          generate(exp)
      val letBody = generate(IRNode.And(cs, next), failureIsPossible || canFail(exp))
      TCN.Let(resVar.getOrElse("_"), letExp, letBody)
    case IRNode.And(Seq(), next) => generate(next, failureIsPossible)
    case IRNode.And(instrs, next) =>
      // Because of the previous cases, when we reach here there's at least one IRInstr.Cond
      val isCond: IRInstr => Boolean = { case IRInstr.Cond(_, _) => true ; case _ => false }
      val conditions = instrs.takeWhile(isCond)
      val remInstrs = instrs.dropWhile(isCond)
      val remNode = generate(IRNode.And(remInstrs, next), failureIsPossible = true)
      
      conditions.foldRight(remNode) { case (IRInstr.Cond(cond, IRError.Generic(err)), elseNode) =>
          TCN.If(codeOps.negate(cond), wrapAsLeft(err), elseNode)
      }

    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = generate(otherwise, failureIsPossible)

      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TCN.If(cond, generate(next, failureIsPossible), elseNode)
      }
  }

  private def wrapAsLeft(value: TCN): TCN = TCN.Apply(TCN.Var("Left"), value)

  private def wrapAsRight(value: TCN): TCN = TCN.Apply(TCN.Var("Right"), value) 
