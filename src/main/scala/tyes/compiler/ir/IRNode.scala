package tyes.compiler.ir

enum IRNode[+TCode]:
  case Unexpected
  case Error(err: TCode)
  case Result(code: TCode, canFail: Boolean)
  case Switch(branches: Seq[(String, IRNode[TCode])], otherwise: IRNode[TCode])
  case And(conds: Seq[IRInstr[TCode]], next: IRNode[TCode])

enum IRInstr[+TCode]:
  case Cond(cond: TCode, err: TCode)
  case Decl(resVar: String, exp: IRNode[TCode])

val example = IRNode.Switch(
  branches = Seq(
    ("_c1 == LNumber(1)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "\"Env must have exactly one identifier\""),
        IRInstr.Cond("env.contains(\"pi\")", "\"Env must contain 'pi'\""),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
        IRInstr.Cond("piT == Type.Real", "\"Expected type of 'pi' to be Real\"")
      ),
      next = IRNode.Result("Type.Real", canFail = false)
    )),
    ("_c1 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "\"Env must have exactly one identifier\""),
        IRInstr.Cond("env.contains(\"pi\")", "\"Env must contain 'pi'\""),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
      ),
      next = IRNode.Result("piT", canFail = false)
    ))
  ),
  otherwise = IRNode.Error("s\"No type for ${_c1}, should be one of: 1, 3\"")
)

val exampleOptimized = IRNode.Switch(
  branches = Seq(
    ("_c1 == LNumber(1) || _c1 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "\"Env must have exactly one identifier\""),
        IRInstr.Cond("env.contains(\"pi\")", "\"Env must contain 'pi'\""),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
        IRInstr.Decl("resT", 
          exp = IRNode.Switch(
            branches = Seq(
              ("_c1 == LNumber(1)", IRNode.And(
                conds = Seq(
                  IRInstr.Cond("piT == Type.Real", "\"Expected type of 'pi' to be Real\"")
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
  otherwise = IRNode.Error("s\"No type for ${_c1}, should be one of: 1, 3\"")
)

val exampleOptimizedV2 = IRNode.Switch(
  branches = Seq(
    ("_c1 == LNumber(1) || _c1 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "\"Env must have exactly one identifier\""),
        IRInstr.Cond("env.contains(\"pi\")", "\"Env must contain 'pi'\""),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")", canFail = false)),
        IRInstr.Decl("resT", 
          exp = IRNode.Switch(
            branches = Seq(
              ("_c1 == LNumber(1)", IRNode.Switch(
                branches = Seq(
                  ("piT == Type.Real", IRNode.Result("Type.Real", canFail = false))
                ),
                otherwise = IRNode.Error("\"Expected type of 'pi' to be Real\"")
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
  otherwise = IRNode.Error("s\"No type for ${_c1}, should be one of: 1, 3\"")
)

val plusExample = IRNode.Switch(
  branches = Seq(
    ("_e2 == LNumber(1)", IRNode.Result("typecheck(e, Map(\"pi\" -> Type.Real))", canFail = true)),
    ("_e2 == LNumber(2)", IRNode.Result("typecheck(e, Map(\"pi\" -> Type.Int))", canFail = true)),
    ("_e2 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Decl("t3", IRNode.Result("typecheck(e, env)", canFail = true)),
        IRInstr.Decl("t4", IRNode.Result("typecheck(LNumber(1), Map(\"pi\" -> t3))", canFail = true)),
        IRInstr.Cond("t4 == t3", "s\"TypeError: types `$t4` and `$t3` don't match\"")
      ),
      next = IRNode.Result("t3", canFail = false)
    ))
  ),
  otherwise = IRNode.Error("s\"TypeError: no type for `LPlus(_, $_e2)`\"")
)

trait CodeOperations[TCode]:
  def negate(code: TCode): TCode

object StringCodeOperations extends CodeOperations[String]:
  def negate(code: String): String =
    if code.contains(' ') 
    then s"!($code)" 
    else s"!$code"      

trait IRNodeCompiler[TCode](
  protected val codeOps: CodeOperations[TCode]
):
  def compile(irNode: IRNode[TCode]): TCode

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

