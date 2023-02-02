package tyes.compiler.ir

enum IRNode[+TCode]:
  case Unexpected
  case Error(err: String)
  case Result(code: TCode, canFail: Boolean)
  case Switch(branches: Seq[(String, IRNode[TCode])], otherwise: IRNode[TCode])
  case And(conds: Seq[IRInstr[TCode]], next: IRNode[TCode])

enum IRInstr[+TCode]:
  case Cond(cond: TCode, err: String)
  case Decl(resVar: String, exp: IRNode[TCode])

val example = IRNode.Switch(
  branches = Seq(
    ("_c1 == LNumber(1)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "Env must have exactly one identifier"),
        IRInstr.Cond("env.contains(\"pi\")", "Env must contain 'pi'"),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
        IRInstr.Cond("piT == Type.Real", "Expected type of 'pi' to be Real")
      ),
      next = IRNode.Result("Type.Real", canFail = false)
    )),
    ("_c1 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "Env must have exactly one identifier"),
        IRInstr.Cond("env.contains(\"pi\")", "Env must contain 'pi'"),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
      ),
      next = IRNode.Result("piT", canFail = false)
    ))
  ),
  otherwise = IRNode.Error("No type for ${_c1}, should be one of: 1, 3")
)

val exampleOptimized = IRNode.Switch(
  branches = Seq(
    ("_c1 == LNumber(1) || _c1 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "Env must have exactly one identifier"),
        IRInstr.Cond("env.contains(\"pi\")", "Env must contain 'pi'"),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
        IRInstr.Decl("resT", 
          exp = IRNode.Switch(
            branches = Seq(
              ("_c1 == LNumber(1)", IRNode.And(
                conds = Seq(
                  IRInstr.Cond("piT == Type.Real", "Expected type of 'pi' to be Real")
                ),
                next = IRNode.Result("Type.Real", canFail = false)
              )),
              ("_c1 == LNumber(3)", IRNode.Result("piT", canFail = false))
            ),
            otherwise = IRNode.Unexpected
          )
        )
      ),
      next = IRNode.Result("resT", canFail = false)  
    ))
  ),
  otherwise = IRNode.Error("No type for ${_c1}, should be one of: 1, 3")
)

val exampleOptimizedV2 = IRNode.Switch(
  branches = Seq(
    ("_c1 == LNumber(1) || _c1 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "Env must have exactly one identifier"),
        IRInstr.Cond("env.contains(\"pi\")", "Env must contain 'pi'"),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
        IRInstr.Decl("resT", 
          exp = IRNode.Switch(
            branches = Seq(
              ("_c1 == LNumber(1)", IRNode.Switch(
                branches = Seq(
                  ("piT == Type.Real", IRNode.Result("Type.Real", canFail = false))
                ),
                otherwise = IRNode.Error("Expected type of 'pi' to be Real")
              )),
              ("_c1 == LNumber(3)", IRNode.Result("piT", canFail = false))
            ),
            otherwise = IRNode.Unexpected
          )
        )
      ),
      next = IRNode.Result("resT", canFail = false)  
    ))
  ),
  otherwise = IRNode.Error("No type for ${_c1}, should be one of: 1, 3")
)

def compile(irNode: IRNode[String]): String = irNode match {
  case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
  case IRNode.Error(err) => s"Left(\"$err\")"
  case IRNode.Result(res, canFail) => if canFail then s"Right($res)" else res
  case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 && canFail(exp) == resCanFail =>
    // Example of special case rule
    compile(IRNode.And(cs, exp))
  case IRNode.And(conds, next) => "for\n" + conds.map(compile).mkString(" ", "\n  ", "\n") + "yield " + compile(next)
  case IRNode.Switch(Seq((cond, IRNode.Result(res, false))), IRNode.Error(err)) =>
    // Example of special case rule
    s"Either.cond($cond, $res, \"$err\")"
  case IRNode.Switch(branches, otherwise) =>
    val needsToHandleFailure = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
    (for (cond, next) <- branches yield
      s"if $cond then\n  " + 
      (if needsToHandleFailure && !canFail(next) then s"Right(${compile(next)})" else compile(next))
    ).mkString("", "\nelse ", "\nelse\n  ") + 
      (if needsToHandleFailure && !canFail(otherwise) then s"Right(${compile(otherwise)})" else compile(otherwise))
}

def compile(irInstr: IRInstr[String]): String = irInstr match {
  case IRInstr.Cond(cond, err) => s"_ <- Either.cond($cond, (), \"$err\")"
  case IRInstr.Decl(resVar, exp) =>
    if canFail(exp) then 
      s"$resVar <- ${compile(exp)}"
    else
      s"$resVar = ${compile(exp)}"
}

// TODO: find better name for needsToHandleFailure param and canFail field
def compileToIfs(irNode: IRNode[String], needsToHandleFailure: Boolean = false): String = irNode match {
  case IRNode.Unexpected => "throw new Exception(\"unexpected\")"
  case IRNode.Error(err) => s"Left(\"$err\")"
  case IRNode.Result(res, canFail) => if canFail || needsToHandleFailure then s"Right($res)" else res
  case IRNode.And(cs :+ IRInstr.Decl(resVar, exp), IRNode.Result(resVar2, resCanFail)) if resVar == resVar2 =>
    // Example of special case rule
    compileToIfs(IRNode.And(cs, exp), needsToHandleFailure || canFail(exp) != resCanFail)
  case IRNode.And(IRInstr.Decl(resVar, exp) +: cs, next) =>
    (if canFail(exp) then
      s"val $resVar = ${compileToIfs(exp)} match { case Right(v) => v ; case left => return left }\n"
    else
      s"val $resVar = ${compileToIfs(exp)}\n") + compileToIfs(IRNode.And(cs, next), needsToHandleFailure)
  case IRNode.And(Seq(), next) => compileToIfs(next, needsToHandleFailure)
  case IRNode.And(instrs, next) =>
    // Because of the previous cases, when we reach here there's at least one IRInstr.Cond
    val isCond: IRInstr[String] => Boolean = { case IRInstr.Cond(_, _) => true ; case _ => false }
    val conditions =
      (
        for case IRInstr.Cond(cond, err) <- instrs.takeWhile(isCond) 
        yield
          val negCond = if cond.contains(' ') then s"!($cond)" else s"!$cond"
          s"if $negCond then Left(\"$err\")"
      ).mkString("", "\nelse ", "\nelse {\n")

    val remInstr = instrs.dropWhile(isCond)
    conditions + compileToIfs(IRNode.And(remInstr, next), needsToHandleFailure) + "\n}"
  case IRNode.Switch(branches, otherwise) =>
    val needsToHandleFailure = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
    (for (cond, next) <- branches yield
      s"if $cond then {\n  " + compileToIfs(next, needsToHandleFailure) + "\n}"
    ).mkString("", " else ", " else {\n") + compileToIfs(otherwise, needsToHandleFailure) + "\n}"
}

def canFail[TCode](irNode: IRNode[TCode]): Boolean = irNode match {
  case IRNode.Unexpected => false
  case IRNode.Result(_, canFail) => canFail
  case IRNode.Error(_) => true
  case IRNode.And(conds, next) => conds.exists(c => canFail(c)) || canFail(next)
  case IRNode.Switch(branches, otherwise) => branches.exists((_, n) => canFail(n)) || canFail(otherwise)
}

def canFail[TCode](irInstr: IRInstr[TCode]): Boolean = irInstr match {
  case IRInstr.Cond(_, _) => true
  case IRInstr.Decl(_, exp) => canFail(exp) 
}