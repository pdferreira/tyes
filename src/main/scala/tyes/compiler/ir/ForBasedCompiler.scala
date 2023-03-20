package tyes.compiler.ir

class ForBasedCompiler extends IRNodeCompiler[String](StringCodeOperations):

  def compile(irNode: IRNode[String]): String = irNode match {
    case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
    case IRNode.Error(err) => s"Left($err)"
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 && canFail(exp) == resCanFail =>
      // Example of special case rule
      compile(IRNode.And(cs, exp))
    case IRNode.And(conds, next) => "for\n" + conds.map(compile).mkString(" ", "\n  ", "\n") + "yield " + compile(next)
    case IRNode.Switch(Seq((cond, IRNode.Result(res, false))), IRNode.Error(err)) =>
      // Example of special case rule
      s"Either.cond($cond, $res, $err)"
    case IRNode.Switch(branches, otherwise) =>
      val needsToHandleFailure = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      (for (cond, next) <- branches yield
        s"if $cond then\n  " + 
        (if needsToHandleFailure && !canFail(next) then s"Right(${compile(next)})" else compile(next))
      ).mkString("", "\nelse ", "\nelse\n  ") + 
        (if needsToHandleFailure && !canFail(otherwise) then s"Right(${compile(otherwise)})" else compile(otherwise))
  }

  def compile(irInstr: IRInstr[String]): String = irInstr match {
    case IRInstr.Cond(cond, err) => s"_ <- Either.cond($cond, (), $err)"
    case IRInstr.Decl(resVar, exp) =>
      if canFail(exp) then 
        s"$resVar <- ${compile(exp)}"
      else
        s"$resVar = ${compile(exp)}"
  }
