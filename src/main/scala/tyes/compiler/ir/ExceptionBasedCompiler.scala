package tyes.compiler.ir

class ExceptionBasedCompiler extends IRNodeCompiler[String](StringCodeOperations):

  def compile(irNode: IRNode[String]): String = irNode match {
    case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
    case IRNode.Error(err) => s"throw new TypeError($err)"
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      compile(IRNode.And(cs, exp))
    case IRNode.And(conds, next) =>
      conds.map(compile).mkString("  ", "\n  ", "\n") + compile(next)
    case IRNode.Switch(branches, otherwise) =>
      (for (cond, next) <- branches yield
        s"if $cond then {\n  " + compile(next) + "\n}"
      ).mkString("", " else ", " else {\n") + compile(otherwise) + "\n}"
  }

  def compile(irInstr: IRInstr[String]): String = irInstr match {
    case IRInstr.Cond(cond, err) =>
      s"if ${codeOps.negate(cond)} then throw new TypeError($err)"
    case IRInstr.Decl(resVar, exp) =>
      s"val $resVar = ${compile(exp)}"
  }
