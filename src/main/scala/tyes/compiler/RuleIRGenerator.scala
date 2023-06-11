package tyes.compiler

import tyes.compiler.ir.IRInstr
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRError
import tyes.compiler.ir.TargetCodeNode
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*
import utils.collections.*

private val TCN = TargetCodeNode

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

  case class GenerateOutput(node: IRNode[TargetCodeNode], condition: Option[TargetCodeNode])

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

  private def genConclusionConds(concl: Judgement, codeEnv: TargetCodeEnv): Seq[IRInstr[TargetCodeNode]] =
    val HasType(cTerm, _) = concl.assertion
    
    val envConds = envIRGenerator.generateConditions(concl.env, codeEnv)
    val termConds = genConclusionTermConds(cTerm, codeEnv)
    termConds ++ envConds

  private def genConclusionTermConds(cTerm: Term, codeEnv: TargetCodeEnv): Seq[IRInstr[TargetCodeNode]] =
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
            case Some(Term.Type(nt @ Type.Named(_))) => 
              val ntCode = typeIRGenerator.generate(nt, codeEnv)
              TCN.Apply(TCN.Field(checkCode, "expecting"), ntCode)
            case _ => checkCode
          },
          canFail = true
        ),
        resVar = Some(realId)
      )

  private def genPremiseConds(premise: Judgement, idx: Int, codeEnv: TargetCodeEnv): Seq[IRInstr[TargetCodeNode]] =
    val HasType(pTerm, pType) = premise.assertion
    pType match {
      case Type.Variable(name) =>
        val (declInstr, _) = genInductionCall(name, pTerm, premise.env, codeEnv) 
        Seq(declInstr)

      case Type.Named(name) =>
        val pTypeCode = typeIRGenerator.generate(pType)
        // TODO: explore having `expecting` as an extra parameter instead, to allow a better error message 
        val (declInstr, _) = genInductionCall("t" + (idx + 1), pTerm, premise.env, codeEnv, indCall => 
          TCN.Apply(TCN.Field(indCall, "expecting"), pTypeCode)
        )
        Seq(declInstr)

      case Type.Composite(tName, args*) =>
        val (innerResTDeclInstr, innerResTDeclVarCode) = genInductionCall("_t" + (idx + 1), pTerm, premise.env, codeEnv)
        val (resTId, resTIdCode) = codeEnv.requestIdentifier("t" + (idx + 1))
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
          innerResTDeclInstr,
          IRInstr.Check(
            exp = IRNode.Result(
              TCN.Apply(
                TCN.Var("cast[Type.$FunType]"), // small hack, just for poc's sake
                innerResTDeclVarCode,
                TCN.FormattedText("expected ", termIRGenerator.generate(pTerm, codeEnv), " to have type ")
              ),
              canFail = true
            ),
            resVar = Some(resTId)
          )
        )
        
        inductionDecls ++ argTypeReqs
    }

  private def genInductionCall(
    declVar: String,
    inductionTerm: Term,
    inductionEnv: Environment,
    codeEnv: TargetCodeEnv,
    transformCall: TargetCodeNode => TargetCodeNode = tcn => tcn 
  ): (IRInstr[TargetCodeNode], TargetCodeNode) =
    val inductionTermCall = termIRGenerator.generate(inductionTerm, codeEnv)
    val (realDeclVar, realDeclVarCode) = codeEnv.requestIdentifier(declVar)
    val instr = IRInstr.Check(
      exp = IRNode.Result(
        transformCall(TCN.Apply(
          TCN.Var("typecheck"),
          inductionTermCall,
          envIRGenerator.generate(inductionEnv, codeEnv)
        )), 
        canFail = true
      ),
      resVar = Some(realDeclVar)
    )
    (instr, realDeclVarCode)
