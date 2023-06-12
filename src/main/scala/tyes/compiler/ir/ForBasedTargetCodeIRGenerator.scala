package tyes.compiler.ir

private val TCN = TargetCodeNode
private val TCTypeRef = TargetCodeTypeRef

class ForBasedTargetCodeIRGenerator extends TargetCodeIRGenerator(TargetCodeNodeOperations):

  def generate(irNode: IRNode): TargetCodeNode = irNode match {
    case IRNode.Unexpected => TCN.Throw(TCTypeRef("Exception"), TCN.Text("unexpected"))
    case IRNode.Error(IRError.Generic(err)) => wrapAsLeft(err)
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Check(exp, Some(resVar)), IRNode.Result(TCN.Var(resVar2), resCanFail)) if resVar == resVar2 && canFail(exp) == resCanFail =>
      // Example of special case rule
      generate(IRNode.And(cs, exp))
    case IRNode.And(conds, next) =>
      TCN.For(conds.map(generate), generate(next))
    case IRNode.Switch(Seq((cond, IRNode.Result(res, false))), IRNode.Error(IRError.Generic(err))) =>
      // Example of special case rule
      TCN.Apply(
        TCN.Field(TCN.Var("Either"), "cond"),
        cond,
        res,
        err
      )
    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = 
        if failureIsPossible && !canFail(otherwise) 
        then wrapAsRight(generate(otherwise))
        else generate(otherwise)
      
      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TCN.If(
          cond,
          if failureIsPossible && !canFail(next) then wrapAsRight(generate(next)) else generate(next),
          elseNode
        )
      }
    case IRNode.Or(main, _) if !canFail(main) => generate(main)
    case IRNode.Or(main, alt) =>
      val mainNode = generate(main)
      val altNode = if !canFail(alt) then wrapAsRight(generate(alt)) else generate(alt)
      TCN.Apply(
        TCN.Field(
          mainNode,
          "orElse"
        ),
        altNode
      )
  }

  def generate(irInstr: IRInstr): TargetCodeForCursor = irInstr match {
    case IRInstr.Cond(cond, IRError.Generic(err)) => 
      val condExp = TCN.Apply(
        TCN.Field(TCN.Var("Either"), "cond"),
        cond,
        TCN.Unit,
        err
      )
      TargetCodeForCursor.Iterate("_", condExp)
    case IRInstr.Check(exp, resVar) =>
      val cursor = resVar.getOrElse("_")
      if canFail(exp) then 
        TargetCodeForCursor.Iterate(cursor, generate(exp))
      else
        TargetCodeForCursor.Let(cursor, generate(exp))
  }

  private def wrapAsLeft(value: TargetCodeNode): TargetCodeNode = TCN.Apply(TCN.Var("Left"), value)

  private def wrapAsRight(value: TargetCodeNode): TargetCodeNode = TCN.Apply(TCN.Var("Right"), value) 