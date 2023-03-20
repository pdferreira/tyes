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
