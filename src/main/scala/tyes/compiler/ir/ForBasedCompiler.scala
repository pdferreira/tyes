package tyes.compiler.ir

class ForBasedStringCompiler extends IRNodeCompiler[String](StringCodeOperations):

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

class ForBasedCodeCompiler extends IRNodeCompiler[CodeGenNode](CodeGenNodeOperations):

  def compile(irNode: IRNode[CodeGenNode]): CodeGenNode = irNode match {
    case IRNode.Unexpected => CodeGenNode.Throw("Exception", CodeGenNode.Text("unexpected"))
    case IRNode.Error(err) => wrapAsLeft(err)
    case IRNode.Result(res, _) => res
    case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(CodeGenNode.Var(resVar2), resCanFail)) if resVar == resVar2 && canFail(exp) == resCanFail =>
      // Example of special case rule
      compile(IRNode.And(cs, exp))
    case IRNode.And(conds, next) =>
      CodeGenNode.For(conds.map(compile), compile(next))
    case IRNode.Switch(Seq((cond, IRNode.Result(res, false))), IRNode.Error(err)) =>
      // Example of special case rule
      CodeGenNode.Apply(
        CodeGenNode.Field(CodeGenNode.Var("Either"), "cond"),
        cond,
        res,
        err
      )
    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = 
        if failureIsPossible && !canFail(otherwise) 
        then wrapAsRight(compile(otherwise))
        else compile(otherwise)
      
      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        CodeGenNode.If(
          cond,
          if failureIsPossible && !canFail(next) then wrapAsRight(compile(next)) else compile(next),
          elseNode
        )
      }
  }

  def compile(irInstr: IRInstr[CodeGenNode]): CodeGenForCursor = irInstr match {
    case IRInstr.Cond(cond, err) => 
      val condExp = CodeGenNode.Apply(
        CodeGenNode.Field(CodeGenNode.Var("Either"), "cond"),
        cond,
        CodeGenNode.Unit,
        err
      )
      CodeGenForCursor.Iterate("_", condExp)
    case IRInstr.Decl(resVar, exp) =>
      if canFail(exp) then 
        CodeGenForCursor.Iterate(resVar, compile(exp))
      else
        CodeGenForCursor.Let(resVar, compile(exp))
  }

  private def wrapAsLeft(value: CodeGenNode): CodeGenNode = CodeGenNode.Apply(CodeGenNode.Var("Left"), value)

  private def wrapAsRight(value: CodeGenNode): CodeGenNode = CodeGenNode.Apply(CodeGenNode.Var("Right"), value) 