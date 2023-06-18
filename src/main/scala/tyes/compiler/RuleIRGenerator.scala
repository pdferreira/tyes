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
            Term.Type(typeIRGenerator.getTempTypeVar(typ, nonVarSuffix = getSuffix(idx)))
        }
      }
      Term.Function(fnName, argsAsVariables*)
    case _ => term
  }

  case class GenerateOutput(node: IRNode, condition: Option[TargetCodeNode])

  def generate(rule: RuleDecl, parentCodeEnv: TargetCodeEnv, overallTemplate: Term): GenerateOutput =
    val HasType(cTerm, cType) = rule.conclusion.assertion

    val codeEnv = new TargetCodeEnv(Some(parentCodeEnv))

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
      .filter((_, v) => v.isGround || v.isInstanceOf[Term.Function])
      .toSeq
      .sortBy((k, v) => k) // TODO: ideally should sort in order of occurrence ltr

    for (k, v) <- constructorReqs
    yield
      if v.isGround then 
        TCN.Equals(TCN.Var(k), termIRGenerator.generate(v))
      else
        val Term.Function(name, _*) = v
        TCN.TypeCheck(TCN.Var(k), TCTypeRef(name))

  private def genConclusionConds(concl: Judgement, codeEnv: TargetCodeEnv): Seq[IRInstr] =
    val HasType(cTerm, _) = concl.assertion
    
    val envConds = envIRGenerator.generateConditions(concl.env, codeEnv)
    val termConds = genConclusionTermConds(cTerm, codeEnv)
    termConds ++ envConds

  private def genConclusionTermConds(cTerm: Term, codeEnv: TargetCodeEnv): Seq[IRInstr] =
    val constructor = extractTemplate(cTerm)
    val termSubst = constructor.matches(cTerm).get
    val typeSubst = termSubst
      .collect({ case (k, Term.Type(typ)) => k -> typ })
      .toMap
    
    val destructureConds = genConclusionDestructureConds(termSubst)
    val typeConds = genConclusionTypeConds(constructor.types.toSeq, typeSubst, codeEnv)
    destructureConds ++ typeConds

  def genConclusionTypeConds(types: Seq[Type], typeSubst: Map[String, Type], codeEnv: TargetCodeEnv): Seq[IRInstr] =
    for
      case t @ Type.Variable(name) <- types 
      if t != Constants.Types.any
    
      checkDeclCode = RuntimeAPIGenerator.genCheckTypeDeclared(codeEnv(t), expVar)
      
      c <- typeSubst.get(name) match {
        case Some(typ) =>      
          // TODO: missing composite type equality checks to bound variables
          val typeCheckDeclCode = typeIRGenerator.generateExpectationCheck(typ, codeEnv, checkDeclCode)
          typeIRGenerator.generateDestructureDecl(typ, codeEnv, IRNode.Result(typeCheckDeclCode, canFail = true))

        case None =>
          Seq(
            IRInstr.Check(
              exp = IRNode.Result(checkDeclCode, canFail = true),
              resPat = 
                val (_, permanentIdCode) = codeEnv.requestIdentifier(typeIRGenerator.getPermanentTypeVar(t))
                TCP.Var(permanentIdCode.name)
            )
          )
      }   
    yield
      c   
  
  def genConclusionDestructureConds(termSubst: Map[String, Term]): Seq[IRInstr] =
    for
      case (k, v: Term.Function) <- termSubst.toSeq
      if !v.isGround
    yield
      IRInstr.Check(
        exp = IRNode.Result(TCN.Var(k), canFail = false),
        resPat = termIRGenerator.generatePattern(v)
      )

  private def genPremiseConds(premise: Judgement, idx: Int, codeEnv: TargetCodeEnv): Seq[IRInstr] =
    val HasType(pTerm, pType) = premise.assertion

    val inductionTerm = typeIRGenerator.generateExpectationCheck(pType, codeEnv, TCN.Apply(
      TCN.Var("typecheck"),
      termIRGenerator.generate(pTerm, codeEnv),
      envIRGenerator.generate(premise.env, codeEnv)
    ))
    
    typeIRGenerator.generateDestructureDecl(pType, codeEnv, IRNode.Result(inductionTerm, canFail = true))
