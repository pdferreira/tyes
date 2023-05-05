package tyes.compiler.ir

class ExceptionBasedStringGenerator extends TargetCodeIRGenerator[String](StringCodeOperations):

  def generate(irNode: IRNode[String]): String = irNode match {
    case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
    case IRNode.Error(err) => s"throw new TypeError($err)"
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      generate(IRNode.And(cs, exp))
    case IRNode.And(conds, next) =>
      conds.map(generate).mkString("  ", "\n  ", "\n") + generate(next)
    case IRNode.Switch(branches, otherwise) =>
      (for (cond, next) <- branches yield
        s"if $cond then {\n  " + generate(next) + "\n}"
      ).mkString("", " else ", " else {\n") + generate(otherwise) + "\n}"
  }

  def generate(irInstr: IRInstr[String]): String = irInstr match {
    case IRInstr.Cond(cond, err) =>
      s"if ${codeOps.negate(cond)} then throw new TypeError($err)"
    case IRInstr.Decl(resVar, exp) =>
      s"val $resVar = ${generate(exp)}"
  }

class ExceptionBasedTargetCodeIRGenerator extends TargetCodeIRGenerator[TargetCodeNode](TargetCodeNodeOperations):

  def generate(irNode: IRNode[TargetCodeNode]): TargetCodeNode = irNode match {
    case IRNode.Unexpected => TargetCodeNode.Throw("Exception", TargetCodeNode.Text("unexpected"))
    case IRNode.Error(err) => TargetCodeNode.Throw("TypeError", err)
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(TargetCodeNode.Var(resVar2), resCanFail)) if resVar == resVar2 =>
      // Example of special case rule
      generate(IRNode.And(cs, exp))
    case IRNode.And(conds, next) =>
      conds.map(generate).foldRight(generate(next)) { case (compose, nextNode) => compose(nextNode) }
    case IRNode.Switch(branches, otherwise) =>
      val otherwiseNode = generate(otherwise)
      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TargetCodeNode.If(cond, generate(next), elseNode)
      }
    case IRNode.Or(main, _) if !canFail(main) => generate(main)
    case IRNode.Or(main, alt) =>
      val mainNode = generate(main)
      val altNode = generate(alt)
      TargetCodeNode.Try(mainNode, "TypeError", altNode)
  }

  def generate(irInstr: IRInstr[TargetCodeNode]): TargetCodeNode => TargetCodeNode = irInstr match {
    case IRInstr.Cond(cond, err) =>
      nextNode => TargetCodeNode.If(codeOps.negate(cond), TargetCodeNode.Throw("TypeError", err), nextNode)
    case IRInstr.Decl(resVar, exp) =>
      nextNode => TargetCodeNode.Let(resVar, generate(exp), nextNode)
  }