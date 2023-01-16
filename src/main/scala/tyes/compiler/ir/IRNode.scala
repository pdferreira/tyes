package tyes.compiler.ir

enum IRNode[+TCode]:
  case Error(err: String)
  case Result(code: TCode)
  case Switch(branches: Seq[(String, IRNode[TCode])], otherwise: IRNode[TCode])
  case And(conds: Seq[IRInstr[TCode]], next: IRNode[TCode])

enum IRInstr[+TCode]:
  case Cond(cond: TCode, err: String)
  case Decl(resVar: String, exp: IRNode[TCode], canFail: Boolean)

val example = IRNode.Switch(
  branches = Seq(
    ("_c1 == LNumber(1)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "Env must have exactly one identifier"),
        IRInstr.Cond("env.contains(\"pi\")", "Env must contain 'pi'"),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")"), canFail = false),
        IRInstr.Cond("piT == Type.Real", "Expected type of 'pi' to be Real")
      ),
      next = IRNode.Result("Type.Real") 
    )),
    ("_c1 == LNumber(3)", IRNode.And(
      conds = Seq(
        IRInstr.Cond("env.size == 1", "Env must have exactly one identifier"),
        IRInstr.Cond("env.contains(\"pi\")", "Env must contain 'pi'"),
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")"), canFail = false),
      ),
      next = IRNode.Result("piT")
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
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")"), canFail = false),
        IRInstr.Decl("resT", 
          exp = IRNode.Switch(
            branches = Seq(
              ("_c1 == LNumber(1)", IRNode.And(
                conds = Seq(
                  IRInstr.Cond("piT == Type.Real", "Expected type of 'pi' to be Real")
                ),
                next = IRNode.Result("Type.Real")
              )),
              ("_c1 == LNumber(3)", IRNode.Result("piT"))
            ),
            otherwise = IRNode.Error("<unexpected>")
          ),
          canFail = true
        )
      ),
      next = IRNode.Result("resT")  
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
        IRInstr.Decl("piT", IRNode.Result("env(\"pi\")"), canFail = false),
        IRInstr.Decl("resT", 
          exp = IRNode.Switch(
            branches = Seq(
              ("_c1 == LNumber(1)", IRNode.Switch(
                branches = Seq(
                  ("piT == Type.Real", IRNode.Result("Type.Real"))
                ),
                otherwise = IRNode.Error("Expected type of 'pi' to be Real")
              )),
              ("_c1 == LNumber(3)", IRNode.Result("piT"))
            ),
            otherwise = IRNode.Error("<unexpected>")
          ),
          canFail = true
        )
      ),
      next = IRNode.Result("resT")  
    ))
  ),
  otherwise = IRNode.Error("No type for ${_c1}, should be one of: 1, 3")
)

def compile(irNode: IRNode[String], canFail: Boolean = true): String = irNode match {
  case IRNode.Error(err) => s"Left(\"$err\")"
  case IRNode.Result(res) => if canFail then s"Right($res)" else res
  case IRNode.And(cs :+ IRInstr.Decl(resVar, exp, false), IRNode.Result(resVar2)) if resVar == resVar2 =>
    // Example of special case rule
    compile(IRNode.And(cs, exp), canFail)
  case IRNode.And(conds, next) => "for\n" + conds.map(compile).mkString(" ", "\n  ", "\n") + "yield " + compile(next, canFail = false)
  case IRNode.Switch(Seq((cond, IRNode.Result(res))), IRNode.Error(err)) =>
    // Example of special case rule
    s"Either.cond($cond, $res, \"$err\")"
  case IRNode.Switch(branches, otherwise) =>
    (for (cond, next) <- branches yield
      s"if $cond then\n  " + compile(next)).mkString("", "\nelse ", "\nelse\n  ") + compile(otherwise)
}

def compile(irInstr: IRInstr[String]): String = irInstr match {
  case IRInstr.Cond(cond, err) => s"_ <- Either.cond($cond, (), \"$err\")"
  case IRInstr.Decl(resVar, exp, canFail) =>
    if canFail then 
      s"$resVar <- ${compile(exp)}"
    else
      s"$resVar = ${compile(exp)}"
}