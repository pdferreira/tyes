package tyes.compiler.ir


enum IRNode[+TCode]:
  case Unexpected
  case Error(err: TCode)
  case Result(code: TCode, canFail: Boolean)
  case Switch(branches: Seq[(TCode, IRNode[TCode])], otherwise: IRNode[TCode])
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

def codeGenLNumber(i: Int) = CodeGenNode.Apply(CodeGenNode.Var("LNumber"), CodeGenNode.Integer(i))

def codeGenCompoundVar(components: String*) = components.foldLeft(None: Option[CodeGenNode]) { (currNode, c) =>
  Some(currNode match {
    case None => CodeGenNode.Var(c)
    case Some(n) => CodeGenNode.Field(n, c)
  })
}.get

def codeGenType(typeName: String) = CodeGenNode.Field(CodeGenNode.Var("Type"), typeName)

val exampleWithCodeGen: IRNode[CodeGenNode] = IRNode.Switch(
  branches = Seq(
    (CodeGenNode.Equals(CodeGenNode.Var("_c1"), codeGenLNumber(1)), IRNode.And(
      conds = Seq(
        IRInstr.Cond(CodeGenNode.Equals(codeGenCompoundVar("env", "size"), CodeGenNode.Integer(1)), CodeGenNode.Text("Env must have exactly one identifier")),
        IRInstr.Cond(CodeGenNode.Apply(codeGenCompoundVar("env", "contains"), CodeGenNode.Text("pi")), CodeGenNode.Text("Env must contain 'pi'")),
        IRInstr.Decl("piT", IRNode.Result(CodeGenNode.Apply(CodeGenNode.Var("env"), CodeGenNode.Text("pi")), canFail = false)),
        IRInstr.Cond(CodeGenNode.Equals(CodeGenNode.Var("piT"), codeGenType("Real")), CodeGenNode.Text("Expected type of 'pi' to be Real"))
      ),
      next = IRNode.Result(codeGenType("Real"), canFail = false)
    )),
    (CodeGenNode.Equals(CodeGenNode.Var("_c1"), codeGenLNumber(3)), IRNode.And(
      conds = Seq(
        IRInstr.Cond(CodeGenNode.Equals(codeGenCompoundVar("env", "size"), CodeGenNode.Integer(1)), CodeGenNode.Text("Env must have exactly one identifier")),
        IRInstr.Cond(CodeGenNode.Apply(codeGenCompoundVar("env", "contains"), CodeGenNode.Text("pi")), CodeGenNode.Text("Env must contain 'pi'")),
        IRInstr.Decl("piT", IRNode.Result(CodeGenNode.Apply(CodeGenNode.Var("env"), CodeGenNode.Text("pi")), canFail = false)),
      ),
      next = IRNode.Result(CodeGenNode.Var("piT"), canFail = false)
    ))
  ),
  otherwise = IRNode.Error(CodeGenNode.FormattedText("No type for ", CodeGenNode.Var("_c1"), ", should be one of: 1, 3"))
)

val numRule = RuleDecl(
  Some("Const"),
  Seq.empty,
  Judgement(
    Environment(Seq(EnvironmentPart.Variable("env"))),
    HasType(Term.Function("LNumber", Term.Constant(1)), Type.Named("const"))
  )
)

val numRuleTargetIR = IRNode.Switch(
  branches = Seq(
    CodeGenNode.Equals(CodeGenNode.Var("c"), CodeGenNode.Integer(1)) -> IRNode.Result(codeGenType("const"), canFail = false)
  ),
  otherwise = IRNode.Error(CodeGenNode.FormattedText("TypeError: no type for ", CodeGenNode.Var("exp")))
)

def extractTemplate(term: Term): Term = term match {
  case Term.Function(fnName, args*) =>
    val argsAsVariables = args.zipWithIndex.map { (arg, idx) =>
      arg match {
        case Term.Variable(_) => arg
        case Term.Constant(_) => Term.Variable("c" + idx)
        case Term.Function(_, _*) => Term.Variable("e" + idx)
        case Term.Type(typ) => Term.Type(typ match {
          case Type.Variable(_) => typ
          case Type.Named(_) => Type.Variable("ct" + idx)
          case Type.Composite(_, _*) => Type.Variable("ct" + idx)
        })
      }
    }
    Term.Function(fnName, argsAsVariables*)
  case _ => term
}

def termToCodeGenNode(term: Term): CodeGenNode = term match {
  case Term.Constant(value: Int) => CodeGenNode.Integer(value)
  case Term.Function(name, args*) => 
    CodeGenNode.Apply(
      CodeGenNode.Var(name),
      args.map(termToCodeGenNode)*
    )
  case Term.Type(typ) => typ match {
    case Type.Named(name) => codeGenType(name)
    case Type.Composite(name, args*) => 
      CodeGenNode.Apply(
        codeGenType(name),
        args.map(Term.Type.apply).map(termToCodeGenNode)*
      )
  }
}

def compileToIR(rule: RuleDecl): IRNode[CodeGenNode] =
  val HasType(cTerm, cType) = rule.conclusion.assertion
  val constructor = extractTemplate(cTerm)
  val constructorReqs = constructor.matches(cTerm).get
  
  IRNode.Switch(
    branches = Seq(
      constructorReqs
        .filter((_, v) => v.isGround)
        .map((k, v) => CodeGenNode.Equals(CodeGenNode.Var(k), termToCodeGenNode(v)))
        .foldLeft(CodeGenNode.Boolean(true))(CodeGenNode.And.apply)
      ->
      IRNode.Result(termToCodeGenNode(Term.Type(cType)), canFail = false)
    ),
    otherwise = IRNode.Error(CodeGenNode.FormattedText("TypeError: no type for ", CodeGenNode.Var("exp")))
  )
       
