package tyes.compiler.ir

class IfBasedStringCompiler extends IRNodeCompiler[String](StringCodeOperations):

  def compile(irNode: IRNode[String]): String = compile(irNode, failureIsPossible = false)

  // TODO: find better name for failureIsPossible param and canFail field
  def compile(irNode: IRNode[String], failureIsPossible: Boolean): String = irNode match {
    case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
    case IRNode.Error(err) => s"Left($err)"
    case IRNode.Result(res, canFail) => if failureIsPossible && !canFail then s"Right($res)" else res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      compile(IRNode.And(cs, exp), failureIsPossible || canFail(exp) != resCanFail)
    case IRNode.And(IRInstr.Decl(resVar, exp) +: cs, next) =>
      (if canFail(exp) then
        s"val $resVar = ${compile(exp)} match { case Right(v) => v ; case left => return left }\n"
      else
        s"val $resVar = ${compile(exp)}\n") + compile(IRNode.And(cs, next), failureIsPossible)
    case IRNode.And(Seq(), next) => compile(next, failureIsPossible)
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
      conditions + compile(IRNode.And(remInstr, next), failureIsPossible) + "\n}"
    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      (for (cond, next) <- branches yield
        s"if $cond then {\n  " + compile(next, failureIsPossible) + "\n}"
      ).mkString("", " else ", " else {\n") + compile(otherwise, failureIsPossible) + "\n}"
  }

class IfBasedCodeCompiler extends IRNodeCompiler[CodeGenNode](CodeGenNodeOperations):

  def compile(irNode: IRNode[CodeGenNode]): CodeGenNode = compile(irNode, failureIsPossible = false)

  // TODO: find better name for failureIsPossible param and canFail field
  def compile(irNode: IRNode[CodeGenNode], failureIsPossible: Boolean): CodeGenNode = irNode match {
    case IRNode.Unexpected => CodeGenNode.Throw("Exception", CodeGenNode.Text("unexpected"))
    case IRNode.Error(err) => wrapAsLeft(err)
    case IRNode.Result(res, canFail) => if failureIsPossible && !canFail then wrapAsRight(res) else res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(CodeGenNode.Var(resVar2), resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      compile(IRNode.And(cs, exp), failureIsPossible || canFail(exp) != resCanFail)
    case IRNode.And(IRInstr.Decl(resVar, exp) +: cs, next) =>
      val letExp = 
        if canFail(exp) then
          CodeGenNode.Match(compile(exp), Seq(
            wrapAsRight(CodeGenNode.Var("v")) -> CodeGenNode.Var("v"),
            CodeGenNode.Var("left") -> CodeGenNode.Return(CodeGenNode.Var("left"))
          ))
        else
          compile(exp)
      val letBody = compile(IRNode.And(cs, next), failureIsPossible)
      CodeGenNode.Let(resVar, letExp, letBody)
    case IRNode.And(Seq(), next) => compile(next, failureIsPossible)
    case IRNode.And(instrs, next) =>
      // Because of the previous cases, when we reach here there's at least one IRInstr.Cond
      val isCond: IRInstr[CodeGenNode] => Boolean = { case IRInstr.Cond(_, _) => true ; case _ => false }
      val conditions = instrs.takeWhile(isCond)
      val remInstrs = instrs.dropWhile(isCond)
      val remNode = compile(IRNode.And(remInstrs, next), failureIsPossible)
      
      conditions.foldRight(remNode) { case (IRInstr.Cond(cond, err), elseNode) =>
          CodeGenNode.If(codeOps.negate(cond), wrapAsLeft(err), elseNode)
      }

    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = compile(otherwise, failureIsPossible)

      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        CodeGenNode.If(cond, compile(next, failureIsPossible), elseNode)
      }
  }

  private def wrapAsLeft(value: CodeGenNode): CodeGenNode = CodeGenNode.Apply(CodeGenNode.Var("Left"), value)

  private def wrapAsRight(value: CodeGenNode): CodeGenNode = CodeGenNode.Apply(CodeGenNode.Var("Right"), value) 
