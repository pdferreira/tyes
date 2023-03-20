package tyes.compiler.ir

class IfBasedCompiler extends IRNodeCompiler[String](StringCodeOperations):

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
