package tyes.compiler.ir

class ForBasedStringGenerator extends TargetCodeIRGenerator[String](StringCodeOperations):

  def generate(irNode: IRNode[String]): String = irNode match {
    case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
    case IRNode.Error(err) => s"Left($err)"
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 && canFail(exp) == resCanFail =>
      // Example of special case rule
      generate(IRNode.And(cs, exp))
    case IRNode.And(conds, next) => "for\n" + conds.map(generate).mkString(" ", "\n  ", "\n") + "yield " + generate(next)
    case IRNode.Switch(Seq((cond, IRNode.Result(res, false))), IRNode.Error(err)) =>
      // Example of special case rule
      s"Either.cond($cond, $res, $err)"
    case IRNode.Switch(branches, otherwise) =>
      val needsToHandleFailure = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      (for (cond, next) <- branches yield
        s"if $cond then\n  " + 
        (if needsToHandleFailure && !canFail(next) then s"Right(${generate(next)})" else generate(next))
      ).mkString("", "\nelse ", "\nelse\n  ") + 
        (if needsToHandleFailure && !canFail(otherwise) then s"Right(${generate(otherwise)})" else generate(otherwise))
  }

  def generate(irInstr: IRInstr[String]): String = irInstr match {
    case IRInstr.Cond(cond, err) => s"_ <- Either.cond($cond, (), $err)"
    case IRInstr.Decl(resVar, exp) =>
      if canFail(exp) then 
        s"$resVar <- ${generate(exp)}"
      else
        s"$resVar = ${generate(exp)}"
  }

class ForBasedTargetCodeIRGenerator extends TargetCodeIRGenerator[TargetCodeNode](TargetCodeNodeOperations):

  def generate(irNode: IRNode[TargetCodeNode]): TargetCodeNode = irNode match {
    case IRNode.Unexpected => TargetCodeNode.Throw(TargetCodeTypeRef("Exception"), TargetCodeNode.Text("unexpected"))
    case IRNode.Error(err) => wrapAsLeft(err)
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(TargetCodeNode.Var(resVar2), resCanFail)) if resVar == resVar2 && canFail(exp) == resCanFail =>
      // Example of special case rule
      generate(IRNode.And(cs, exp))
    case IRNode.And(conds, next) =>
      TargetCodeNode.For(conds.map(generate), generate(next))
    case IRNode.Switch(Seq((cond, IRNode.Result(res, false))), IRNode.Error(err)) =>
      // Example of special case rule
      TargetCodeNode.Apply(
        TargetCodeNode.Field(TargetCodeNode.Var("Either"), "cond"),
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
        TargetCodeNode.If(
          cond,
          if failureIsPossible && !canFail(next) then wrapAsRight(generate(next)) else generate(next),
          elseNode
        )
      }
    case IRNode.Or(main, _) if !canFail(main) => generate(main)
    case IRNode.Or(main, alt) =>
      val mainNode = generate(main)
      val altNode = if !canFail(alt) then wrapAsRight(generate(alt)) else generate(alt)
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
        TargetCodeForCursor.Iterate(resVar, generate(exp))
      else
        TargetCodeForCursor.Let(resVar, generate(exp))
  }

  private def wrapAsLeft(value: TargetCodeNode): TargetCodeNode = TargetCodeNode.Apply(TargetCodeNode.Var("Left"), value)

  private def wrapAsRight(value: TargetCodeNode): TargetCodeNode = TargetCodeNode.Apply(TargetCodeNode.Var("Right"), value) 