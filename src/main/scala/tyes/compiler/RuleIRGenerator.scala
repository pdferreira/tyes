package tyes.compiler

import tyes.compiler.ir.IRInstr
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRError
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*
import utils.collections.*

private val TCN = TargetCodeNode
private val TCP = TargetCodePattern

class RuleIRGenerator(
  private val typeIRGenerator: TypeIRGenerator,
  private val termIRGenerator: TermIRGenerator,
  private val envIRGenerator: EnvironmentIRGenerator,
  private val expVar: TCN.Var
):

  def getTemplate(rule: RuleDecl): Term = rule.conclusion.assertion match {
    case HasType(term, _) => extractTemplate(term) 
  }

  private def getTempId(desiredId: String) = "_" + desiredId

  private def getPermanentId(tempId: String) =
    assert(tempId.startsWith("_"), s"Expected temporary id to be provided: $tempId")
    tempId.substring(1)

  private def extractTemplate(term: Term): Term = term match {
    case Term.Function(fnName, args*) =>
      def getSuffix(idx: Int): String =
        if args.length == 1 
        then "" 
        else (idx + 1).toString
  
      val argsAsVariables = args.zipWithIndex.map { (arg, idx) =>
        arg match {
          case Term.Variable(_) => arg
          case Term.Constant(_) =>
            // Simple naming heuristic based on the constructor name, while field
            // names are not considered.
            val initial = fnName.findLast(_.isUpper).map(_.toLower).getOrElse('c')
            Term.Variable(initial + getSuffix(idx))
          case Term.Function(_, _*) => Term.Variable("e" + getSuffix(idx))
          case Term.Type(typ) => 
            // Type variable arguments are assumed to be optional, so we match
            // them with a temporary name and only later declare them with their original
            // name when checked for content
            Term.Type(typ match {
              case Constants.Types.any => typ
              case Type.Variable(name) => Type.Variable(getTempId(name))
              case Type.Named(_) => Type.Variable(getTempId("ct") + getSuffix(idx))
              case Type.Composite(_, _*) => Type.Variable(getTempId("ct") + getSuffix(idx))
            })
        }
      }
      Term.Function(fnName, argsAsVariables*)
    case _ => term
  }

  case class GenerateOutput(node: IRNode, condition: Option[TargetCodeNode])

  def generate(rule: RuleDecl, parentCodeEnv: TargetCodeEnv, overallTemplate: Term): GenerateOutput =
    val HasType(cTerm, cType) = rule.conclusion.assertion

    val codeEnv = new TargetCodeEnv(Some(parentCodeEnv))

    // Pre-register the simple names for the types in the conclusion and the premises
    for case Type.Variable(name) <- cTerm.types do
      codeEnv.requestIdentifier(name)

    for case Judgement(_, HasType(_, Type.Variable(name))) <- rule.premises do
      codeEnv.requestIdentifier(name)

    val conclusionConds = genConclusionConds(rule.conclusion, codeEnv)

    val premiseConds = rule.premises
      .zipWithIndex
      .flatMap((p, idx) => genPremiseConds(p, idx, codeEnv))

    val conds = conclusionConds ++ premiseConds

    var result = IRNode.Result(typeIRGenerator.generate(cType, codeEnv), canFail = false)
    
    if !conds.isEmpty then
      result = IRNode.And(
        conds = conds.toSeq,
        next = result
      )

    val constructorReqs = genConstructorReqs(cTerm)

    GenerateOutput(
      node = result,
      condition = constructorReqs
        .nonEmptyOption
        .map(_.foldLeft1(TCN.And.apply))
    )

  private def genConstructorReqs(term: Term): Iterable[TargetCodeNode] =
    val constructor = extractTemplate(term)
    val constructorReqs = constructor.matches(term)
      .get
      .filter((_, v) => v.isGround)
      .toSeq
      .sortBy((k, v) => k) // TODO: ideally should sort in order of occurrence ltr

    for (k, v) <- constructorReqs
      yield TCN.Equals(TCN.Var(k), termIRGenerator.generate(v))

  private def genConclusionConds(concl: Judgement, codeEnv: TargetCodeEnv): Seq[IRInstr] =
    val HasType(cTerm, _) = concl.assertion
    
    val envConds = envIRGenerator.generateConditions(concl.env, codeEnv)
    val termConds = genConclusionTermConds(cTerm, codeEnv)
    termConds ++ envConds

  private def genConclusionTermConds(cTerm: Term, codeEnv: TargetCodeEnv): Seq[IRInstr] =
    val constructor = extractTemplate(cTerm)
    val termSubst = constructor.matches(cTerm).get
    for
      case t @ Type.Variable(name) <- constructor.types.toSeq 
      if t != Constants.Types.any
    yield
      val (realTmpId, realTmpIdCode) = codeEnv.requestIdentifier(name)
      val (realId, _) = codeEnv.requestIdentifier(getPermanentId(realTmpId))
      val checkCode = RuntimeAPIGenerator.genCheckTypeDeclared(realTmpIdCode, expVar)
      IRInstr.Check(
        exp = IRNode.Result(
          termSubst.get(name) match {
            case Some(Term.Type(typ)) => genTypeExpectationCheck(typ, codeEnv, checkCode)
            case _ => checkCode
          },
          canFail = true
        ),
        resVar = Some(realId)
      )

  private def genPremiseConds(premise: Judgement, idx: Int, codeEnv: TargetCodeEnv): Seq[IRInstr] =
    val HasType(pTerm, pType) = premise.assertion
    pType match {
      case Type.Variable(name) =>
        val (declInstr, _) = genInductionCall(name, premise, codeEnv) 
        Seq(declInstr)

      case Type.Named(name) =>
        val pTypeCode = typeIRGenerator.generate(pType)
        val (declInstr, _) = genInductionCall("t" + (idx + 1), premise, codeEnv)
        Seq(declInstr)

      case Type.Composite(tName, args*) =>
        val (resTDeclInstr, resTIdCode) = genInductionCall("t" + (idx + 1), premise, codeEnv)

        val argTypeReqs = args
          .zipWithIndex
          .withFilter((arg, argIdx) => arg match {
            case Type.Variable(name) => codeEnv.contains(name)
            case _ => true
          })
          .map((arg, argIdx) => {
            val leftTypeNode = TCN.Field(resTIdCode, "t" + (argIdx + 1))
            val rightTypeNode = typeIRGenerator.generate(arg, codeEnv)
            IRInstr.Cond(
              TCN.Equals(leftTypeNode, rightTypeNode),
              IRError.UnexpectedType(expected = leftTypeNode, obtained = rightTypeNode)
            )
          })
        
        for case (Type.Variable(name), argIdx) <- args.zipWithIndex if !codeEnv.contains(name) do
          codeEnv.registerIdentifier(name, TCN.Field(resTIdCode, "t" + (argIdx + 1)))
        
        val inductionDecls = Seq(
          resTDeclInstr
        )
        
        inductionDecls ++ argTypeReqs
    }

  private def genTypeExpectationCheck(typ: Type, codeEnv: TargetCodeEnv, typeProviderCode: TCN): TargetCodeNode = typ match {
    case Type.Named(_) =>
      val typeCode = typeIRGenerator.generate(typ, codeEnv)
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeCode)
    
    case Type.Composite(name, _*) =>
      val typeRef = typeIRGenerator.generateRef(name)
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeRef)

    case _ =>
      // Nothing to check
      typeProviderCode
  }

  private def genInductionCall(
    declVar: String,
    premise: Judgement,
    codeEnv: TargetCodeEnv,
  ): (IRInstr, TargetCodeNode) =
    val HasType(pTerm, pType) = premise.assertion

    val inductionTermCall = termIRGenerator.generate(pTerm, codeEnv)
    val (realDeclVar, realDeclVarCode) = codeEnv.requestIdentifier(declVar)
    val instr = IRInstr.Check(
      exp = IRNode.Result(
        genTypeExpectationCheck(pType, codeEnv, TCN.Apply(
          TCN.Var("typecheck"),
          inductionTermCall,
          envIRGenerator.generate(premise.env, codeEnv)
        )), 
        canFail = true
      ),
      resVar = Some(realDeclVar)
    )
    (instr, realDeclVarCode)
