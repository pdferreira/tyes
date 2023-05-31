package tyes.compiler.ir

class TargetCodeIRGeneratorImpl extends TargetCodeIRGenerator[TargetCodeNode](TargetCodeNodeOperations):

  def generate(irNode: IRNode[TargetCodeNode]): TargetCodeNode = generate(irNode, eitherIsExpected = true)
    
  private def generate(irNode: IRNode[TargetCodeNode], eitherIsExpected: Boolean): TargetCodeNode = irNode match {
    case IRNode.Unexpected => 
      TargetCodeNode.Throw(TargetCodeTypeRef("Exception"), TargetCodeNode.Text("unexpected"))
    
    case IRNode.Error(err) =>
      assert(eitherIsExpected, "Returning Either must be expected in Error")
      wrapAsLeft(err)
    
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
      // Example of special case rule
      generate(IRNode.And(cs, exp), eitherIsExpected)
    
    case IRNode.And(conds, next) =>
      assert(eitherIsExpected, "Returning Either must be expected in Ands")
      TargetCodeNode.For(conds.map(generate), generate(next, eitherIsExpected = false))
    
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
      val altNode = generate(alt, eitherIsExpected = true)//if !canFail(alt) then wrapAsRight(generate(alt)) else generate(alt)
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
      val condExp = TargetCodeNode.Apply(
        TargetCodeNode.Field(TargetCodeNode.Var("Either"), "cond"),
        cond,
        TargetCodeNode.Unit,
        err
      )
      TargetCodeForCursor.Iterate("_", condExp)
    case IRInstr.Decl(resVar, exp) =>
      if canFail(exp) then 
        TargetCodeForCursor.Iterate(resVar, generate(exp, eitherIsExpected = true))
      else
        TargetCodeForCursor.Let(resVar, generate(exp, eitherIsExpected = false))
  }

  private def wrapAsLeft(value: TargetCodeNode): TargetCodeNode = TargetCodeNode.Apply(TargetCodeNode.Var("Left"), value)

  private def wrapAsRight(value: TargetCodeNode): TargetCodeNode = TargetCodeNode.Apply(TargetCodeNode.Var("Right"), value) 