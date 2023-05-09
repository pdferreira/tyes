package tyes.compiler.ir

class IfBasedStringGenerator extends TargetCodeIRGenerator[String](StringCodeOperations):

  def generate(irNode: IRNode[String]): String = generate(irNode, failureIsPossible = false)

  // TODO: find better name for failureIsPossible param and canFail field
  def generate(irNode: IRNode[String], failureIsPossible: Boolean): String = irNode match {
    case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
    case IRNode.Error(err) => s"Left($err)"
    case IRNode.Result(res, canFail) => if failureIsPossible && !canFail then s"Right($res)" else res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      generate(IRNode.And(cs, exp), failureIsPossible || canFail(exp) != resCanFail)
    case IRNode.And(IRInstr.Decl(resVar, exp) +: cs, next) =>
      (if canFail(exp) then
        s"val $resVar = ${generate(exp)} match { case Right(v) => v ; case left => return left }\n"
      else
        s"val $resVar = ${generate(exp)}\n") + generate(IRNode.And(cs, next), failureIsPossible)
    case IRNode.And(Seq(), next) => generate(next, failureIsPossible)
    case IRNode.And(instrs, next) =>
      // Because of the previous cases, when we reach here there's at least one IRInstr.Cond
      val isCond: IRInstr[String] => Boolean = { case IRInstr.Cond(_, _) => true ; case _ => false }
      val conditions =
        (
          for case IRInstr.Cond(cond, err) <- instrs.takeWhile(isCond) 
          yield
            s"if ${codeOps.negate(cond)} then Left($err)"
        ).mkString("", "\nelse ", "\nelse {\n")

      val remInstr = instrs.dropWhile(isCond)
      conditions + generate(IRNode.And(remInstr, next), failureIsPossible) + "\n}"
    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      (for (cond, next) <- branches yield
        s"if $cond then {\n  " + generate(next, failureIsPossible) + "\n}"
      ).mkString("", " else ", " else {\n") + generate(otherwise, failureIsPossible) + "\n}"
  }

class IfBasedTargetCodeIRGenerator extends TargetCodeIRGenerator[TargetCodeNode](TargetCodeNodeOperations):

  def generate(irNode: IRNode[TargetCodeNode]): TargetCodeNode = generate(irNode, failureIsPossible = false)

  // TODO: find better name for failureIsPossible param and canFail field
  def generate(irNode: IRNode[TargetCodeNode], failureIsPossible: Boolean): TargetCodeNode = irNode match {
    case IRNode.Unexpected => TargetCodeNode.Throw(TargetCodeTypeRef("Exception"), TargetCodeNode.Text("unexpected"))
    case IRNode.Error(err) => wrapAsLeft(err)
    case IRNode.Result(res, canFail) => if failureIsPossible && !canFail then wrapAsRight(res) else res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(TargetCodeNode.Var(resVar2), resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      generate(IRNode.And(cs, exp), failureIsPossible || canFail(exp) != resCanFail)
    case IRNode.And(IRInstr.Decl(resVar, exp) +: cs, next) =>
      val letExp = 
        if canFail(exp) then
          TargetCodeNode.Match(generate(exp), Seq(
            wrapAsRight(TargetCodeNode.Var("v")) -> TargetCodeNode.Var("v"),
            TargetCodeNode.Var("left") -> TargetCodeNode.Return(TargetCodeNode.Var("left"))
          ))
        else
          generate(exp)
      val letBody = generate(IRNode.And(cs, next), failureIsPossible || canFail(exp))
      TargetCodeNode.Let(resVar, letExp, letBody)
    case IRNode.And(Seq(), next) => generate(next, failureIsPossible)
    case IRNode.And(instrs, next) =>
      // Because of the previous cases, when we reach here there's at least one IRInstr.Cond
      val isCond: IRInstr[TargetCodeNode] => Boolean = { case IRInstr.Cond(_, _) => true ; case _ => false }
      val conditions = instrs.takeWhile(isCond)
      val remInstrs = instrs.dropWhile(isCond)
      val remNode = generate(IRNode.And(remInstrs, next), failureIsPossible = true)
      
      conditions.foldRight(remNode) { case (IRInstr.Cond(cond, err), elseNode) =>
          TargetCodeNode.If(codeOps.negate(cond), wrapAsLeft(err), elseNode)
      }

    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = generate(otherwise, failureIsPossible)

      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TargetCodeNode.If(cond, generate(next, failureIsPossible), elseNode)
      }
  }

  private def wrapAsLeft(value: TargetCodeNode): TargetCodeNode = TargetCodeNode.Apply(TargetCodeNode.Var("Left"), value)

  private def wrapAsRight(value: TargetCodeNode): TargetCodeNode = TargetCodeNode.Apply(TargetCodeNode.Var("Right"), value) 
