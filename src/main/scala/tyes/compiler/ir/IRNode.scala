package tyes.compiler.ir

import tyes.model.*

enum IRNode[+TCode]:
  case Unexpected
  case Error(err: TCode)
  case Result(code: TCode, canFail: Boolean)
  case Switch(branches: Seq[(TCode, IRNode[TCode])], otherwise: IRNode[TCode])
  case And(conds: Seq[IRInstr[TCode]], next: IRNode[TCode])
  case Or(main: IRNode[TCode], alternative: IRNode[TCode])

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

def codeGenLNumber(i: Int) = TargetCodeNode.Apply(TargetCodeNode.Var("LNumber"), TargetCodeNode.Integer(i))

def codeGenCompoundVar(components: String*) = components.foldLeft(None: Option[TargetCodeNode]) { (currNode, c) =>
  Some(currNode match {
    case None => TargetCodeNode.Var(c)
    case Some(n) => TargetCodeNode.Field(n, c)
  })
}.get

def codeGenType(typeName: String) = TargetCodeNode.Field(TargetCodeNode.Var("Type"), typeName)

val exampleWithCodeGen: IRNode[TargetCodeNode] = IRNode.Switch(
  branches = Seq(
    (TargetCodeNode.Equals(TargetCodeNode.Var("_c1"), codeGenLNumber(1)), IRNode.And(
      conds = Seq(
        IRInstr.Cond(TargetCodeNode.Equals(codeGenCompoundVar("env", "size"), TargetCodeNode.Integer(1)), TargetCodeNode.Text("Env must have exactly one identifier")),
        IRInstr.Cond(TargetCodeNode.Apply(codeGenCompoundVar("env", "contains"), TargetCodeNode.Text("pi")), TargetCodeNode.Text("Env must contain 'pi'")),
        IRInstr.Decl("piT", IRNode.Result(TargetCodeNode.Apply(TargetCodeNode.Var("env"), TargetCodeNode.Text("pi")), canFail = false)),
        IRInstr.Cond(TargetCodeNode.Equals(TargetCodeNode.Var("piT"), codeGenType("Real")), TargetCodeNode.Text("Expected type of 'pi' to be Real"))
      ),
      next = IRNode.Result(codeGenType("Real"), canFail = false)
    )),
    (TargetCodeNode.Equals(TargetCodeNode.Var("_c1"), codeGenLNumber(3)), IRNode.And(
      conds = Seq(
        IRInstr.Cond(TargetCodeNode.Equals(codeGenCompoundVar("env", "size"), TargetCodeNode.Integer(1)), TargetCodeNode.Text("Env must have exactly one identifier")),
        IRInstr.Cond(TargetCodeNode.Apply(codeGenCompoundVar("env", "contains"), TargetCodeNode.Text("pi")), TargetCodeNode.Text("Env must contain 'pi'")),
        IRInstr.Decl("piT", IRNode.Result(TargetCodeNode.Apply(TargetCodeNode.Var("env"), TargetCodeNode.Text("pi")), canFail = false)),
      ),
      next = IRNode.Result(TargetCodeNode.Var("piT"), canFail = false)
    ))
  ),
  otherwise = IRNode.Error(TargetCodeNode.FormattedText("No type for ", TargetCodeNode.Var("_c1"), ", should be one of: 1, 3"))
)

val numRule = RuleDecl(
  Some("Const"),
  Seq.empty,
  Judgement(
    Environment(Seq(EnvironmentPart.Variable("env"))),
    HasType(Term.Function("LNumber", Term.Constant(1)), Type.Named("const"))
  )
)

val altNumRule = RuleDecl(
  Some("Const2"),
  Seq.empty,
  Judgement(
    Environment(Seq(EnvironmentPart.Variable("env"))),
    HasType(Term.Function("LNumber", Term.Constant(2)), Type.Named("const"))
  )
)

val conditionalNumRule = RuleDecl(
  Some("ConstCond"),
  Seq(
    Judgement(
      Environment(Seq(EnvironmentPart.Variable("env"))),
      HasType(Term.Function("LVariable", Term.Constant("num")), Type.Named("const"))
    )
  ),
  Judgement(
    Environment(Seq(EnvironmentPart.Variable("env"))),
    HasType(Term.Function("LNumber", Term.Variable("n")), Type.Named("const"))
  )
)

val appRule = RuleDecl(
  Some("App"),
  Seq(
    Judgement(
      Environment(Seq(EnvironmentPart.Variable("env"))),
      HasType(Term.Variable("e1"), Type.Composite("$Fun", Type.Variable("a"), Type.Variable("b")))
    ),
    Judgement(
      Environment(Seq(EnvironmentPart.Variable("env"))),
      HasType(Term.Variable("e2"), Type.Variable("a"))
    )
  ),
  Judgement(
    Environment(Seq(EnvironmentPart.Variable("env"))),
    HasType(Term.Function("LApp", Term.Variable("e1"), Term.Variable("e2")), Type.Variable("b"))
  )
)

val revAppRule = RuleDecl(
  Some("RevApp"),
  Seq(
    Judgement(
      Environment(Seq(EnvironmentPart.Variable("env"))),
      HasType(Term.Variable("v"), Type.Variable("b"))
    ),
    Judgement(
      Environment(Seq(EnvironmentPart.Variable("env"))),
      HasType(Term.Variable("f"), Type.Composite("$Fun", Type.Variable("b"), Type.Variable("a")))
    )
  ),
  Judgement(
    Environment(Seq(EnvironmentPart.Variable("env"))),
    HasType(Term.Function("LApp", Term.Variable("v"), Term.Variable("f")), Type.Variable("a"))
  )
)

val exampleTypeSystem = TypeSystemDecl(None, Seq(numRule, appRule))

def extractTemplate(term: Term): Term = term match {
  case Term.Function(fnName, args*) =>
    val argsAsVariables = args.zipWithIndex.map { (arg, idx) =>
      arg match {
        case Term.Variable(_) => arg
        case Term.Constant(_) => Term.Variable("c" + (idx + 1))
        case Term.Function(_, _*) => Term.Variable("e" + (idx + 1))
        case Term.Type(typ) => Term.Type(typ match {
          case Type.Variable(_) => typ
          case Type.Named(_) => Type.Variable("ct" + (idx + 1))
          case Type.Composite(_, _*) => Type.Variable("ct" + (idx + 1))
        })
      }
    }
    Term.Function(fnName, argsAsVariables*)
  case _ => term
}

def termToCodeGenNode(term: Term, codeEnv: Map[String, TargetCodeNode] = Map()): TargetCodeNode = term match {
  case Term.Constant(value: Int) => TargetCodeNode.Integer(value)
  case Term.Constant(value: String) => TargetCodeNode.Text(value)
  case Term.Variable(name) => codeEnv.getOrElse(name, TargetCodeNode.Var(name))
  case Term.Function(name, args*) => 
    TargetCodeNode.Apply(
      TargetCodeNode.Var(name),
      args.map(termToCodeGenNode(_, codeEnv))*
    )
  case Term.Type(typ) => typ match {
    case Type.Named(name) => codeGenType(name)
    case Type.Variable(name) => codeEnv.getOrElse(name, TargetCodeNode.Var(name))
    case Type.Composite(name, args*) => 
      TargetCodeNode.Apply(
        codeGenType(name),
        args.map(Term.Type.apply).map(termToCodeGenNode(_, codeEnv))*
      )
  }
}

import tyes.compiler.TargetCodeEnv
import utils.collections.*

def compileInductionToIR(declVar: String, inductionTerm: Term, codeEnv: TargetCodeEnv): (IRInstr[TargetCodeNode], TargetCodeNode) =
  val preDeclEnv = codeEnv.toMap
  val (realDeclVar, realDeclVarCode) = codeEnv.requestIdentifier(declVar)
  val instr = IRInstr.Decl(
    realDeclVar,
    IRNode.Result(
      TargetCodeNode.Apply(
        TargetCodeNode.Var("typecheck"),
        termToCodeGenNode(inductionTerm, preDeclEnv),
        TargetCodeNode.Var("env")
      ), 
      canFail = true
    )
  )
  (instr, realDeclVarCode)

def getConclusionTerm(rule: RuleDecl): Term = rule.conclusion.assertion.asInstanceOf[HasType].term

def compileToIR(rules: Seq[RuleDecl]): IRNode[TargetCodeNode] =
  val distinctConstructors = rules
    .map(getConclusionTerm)
    .map(extractTemplate)
    .combinations(2)
    .filterNot(cs => cs(0).overlaps(cs(1)))
    .flatten
    .toSeq

  assert(distinctConstructors.isEmpty, s"Expected all rules to share their constructor: $distinctConstructors")

  val overallTemplate = extractTemplate(getConclusionTerm(rules.head)).asInstanceOf[Term.Function]
  val commonCodeEnv = new TargetCodeEnv
  for v <- overallTemplate.variables do
    commonCodeEnv.registerIdentifier(v, TargetCodeNode.Var(v))

  if getConclusionTerm(rules(0)).overlaps(getConclusionTerm(rules(1))) then
    rules.map(r => compileToIR(r, commonCodeEnv, overallTemplate)).foldLeft1(IRNode.Or.apply)
  else
    // If they do not overlap, there's at least one fixed criteria that leads to a switch
    // in one of them
    val irNode0 = compileToIR(rules(0), commonCodeEnv, overallTemplate)
    val irNode1 = compileToIR(rules(1), commonCodeEnv, overallTemplate)
    val resNode = (irNode0, irNode1) match { 
      case (IRNode.Switch(bs, _), _) => IRNode.Switch(bs, irNode1) 
      case (_, IRNode.Switch(bs, _)) => IRNode.Switch(bs, irNode0)
      case _ => ???
    }
    if rules.length == 2 then
      resNode
    else if getConclusionTerm(rules(0)).overlaps(getConclusionTerm(rules(2)))
      || getConclusionTerm(rules(1)).overlaps(getConclusionTerm(rules(2))) 
    then
      val irNode2 = compileToIR(rules(2), commonCodeEnv, overallTemplate)
      IRNode.Or(resNode, irNode2)
    else
      val irNode2 = compileToIR(rules(2), commonCodeEnv, overallTemplate)
      (resNode, irNode2) match { 
        case (IRNode.Switch(bs, _), _) => IRNode.Switch(bs, irNode2) 
        case (_, IRNode.Switch(bs, _)) => IRNode.Switch(bs, resNode)
        case _ => ???
      }

def compileToIR(rule: RuleDecl, parentCodeEnv: TargetCodeEnv, overallTemplate: Term.Function): IRNode[TargetCodeNode] =
  val HasType(cTerm, cType) = rule.conclusion.assertion
  
  val constructor = extractTemplate(cTerm)
  val codeEnv = new TargetCodeEnv(Some(parentCodeEnv))
  
  for case (name, Term.Variable(varName)) <- overallTemplate.matches(constructor).get do
    if name != varName then
      codeEnv.registerIdentifier(varName, codeEnv(name))

  // Pre-register the simple type names for the premises 
  for case Judgement(_, HasType(_, Type.Variable(name))) <- rule.premises do
    codeEnv.requestIdentifier(name)

  val premiseReqs = rule.premises
    .zipWithIndex
    .map((j, idx) => {
      val HasType(pTerm, pType) = j.assertion
      pType match {
        case Type.Variable(name) =>
          val (declInstr, _) = compileInductionToIR(name, pTerm, codeEnv) 
          (Seq(declInstr), Seq.empty)
        case Type.Named(name) =>
          val (declInstr, declVarCode) = compileInductionToIR("t" + (idx + 1), pTerm, codeEnv)
          val cond = IRInstr.Cond(
            TargetCodeNode.Equals(declVarCode, codeGenType(name)),
            TargetCodeNode.FormattedText("TypeError: types ", codeGenType(name), " and ", declVarCode, " don't match")
          )
          (Seq(declInstr), Seq(cond))
        case Type.Composite("$Fun", args*) =>
          val (innerResTDeclInstr, innerResTDeclVarCode) = compileInductionToIR("_t" + (idx + 1), pTerm, codeEnv)
          val (resTId, resTIdCode) = codeEnv.requestIdentifier("t" + (idx + 1))
          val argTypeReqs = args
            .zipWithIndex
            .withFilter((arg, argIdx) => arg match {
              case Type.Variable(name) => codeEnv.contains(name)
              case _ => true
            })
            .map((arg, argIdx) => {
              val leftTypeNode = TargetCodeNode.Field(resTIdCode, "t" + (argIdx + 1))
              val rightTypeNode = termToCodeGenNode(Term.Type(arg), codeEnv.toMap)
              IRInstr.Cond(
                TargetCodeNode.Equals(leftTypeNode, rightTypeNode),
                TargetCodeNode.FormattedText("TypeError: types ", leftTypeNode, " and ", rightTypeNode, " don't match")
              )
            })
          
          for case (Type.Variable(name), argIdx) <- args.zipWithIndex if !codeEnv.contains(name) do
            codeEnv.registerIdentifier(name, TargetCodeNode.Field(resTIdCode, "t" + (argIdx + 1)))

          val inductionDecls = Seq(
            innerResTDeclInstr,
            IRInstr.Decl(
              resTId,
              IRNode.Result(
                TargetCodeNode.Apply(
                  TargetCodeNode.Var("cast[Type.$Fun]"), // small hack, just for poc's sake
                  innerResTDeclVarCode,
                  TargetCodeNode.FormattedText("expected ", termToCodeGenNode(pTerm, codeEnv.toMap), " to have type ")
                ),
                canFail = true
              )
            )
          )

          (inductionDecls, argTypeReqs)
      }
    })

  println(codeEnv)
  var result = IRNode.Result(termToCodeGenNode(Term.Type(cType), codeEnv.toMap), canFail = false)
  
  if !premiseReqs.isEmpty then
    result = IRNode.And(
      conds = premiseReqs.flatMap(_._1) ++ premiseReqs.flatMap(_._2),
      next = result
    )

  val constructorReqs = constructor.matches(cTerm)
    .get
    .filter((_, v) => v.isGround)
  
  if constructorReqs.isEmpty then
    result
  else
    IRNode.Switch(
      branches = Seq(
        constructorReqs
          .map((k, v) => TargetCodeNode.Equals(TargetCodeNode.Var(k), termToCodeGenNode(v)))
          .foldLeft1(TargetCodeNode.And.apply)
        ->
        result
      ),
      otherwise = IRNode.Error(TargetCodeNode.FormattedText("TypeError: no type for ", TargetCodeNode.Var("exp")))
    )
