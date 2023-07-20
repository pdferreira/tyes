package tyes.compiler

import tyes.compiler.ir.IRCond
import tyes.compiler.ir.IRError
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRType
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

  case class GenerateOutput(node: IRNode, condition: Option[IRCond])

  def generate(rule: RuleDecl, parentCodeEnv: TargetCodeEnv, overallTemplate: Term): GenerateOutput =
    val HasType(cTerm, cType) = rule.conclusion.assertion

    val codeEnv = new TargetCodeEnv(Some(parentCodeEnv))

    val conclusionConds = genConclusionConds(rule.conclusion, codeEnv)

    val premiseConds = rule.premises
      .zipWithIndex
      .flatMap((p, idx) => genPremiseConds(p, idx, codeEnv))

    val conds = conclusionConds ++ premiseConds

    var result = IRNode.Type(IRType.FromCode(typeIRGenerator.generate(cType, codeEnv)))
    
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
        .map(_.foldLeft1(IRCond.And.apply))
    )

  private def genConstructorReqs(term: Term): Iterable[IRCond] =
    val constructor = extractTemplate(term)
    val constructorReqs = constructor.matches(term)
      .get
      .filter((_, v) => v.isGround || v.isInstanceOf[Term.Function])
      .toSeq
      .sortBy((k, v) => k) // TODO: ideally should sort in order of occurrence ltr

    for (k, v) <- constructorReqs
    yield
      if v.isGround then 
        IRCond.TermEquals(TCN.Var(k), termIRGenerator.generate(v))
      else
        val Term.Function(name, _*) = v
        IRCond.OfType(TCN.Var(k), TCTypeRef(name))

  private def genConclusionConds(concl: Judgement, codeEnv: TargetCodeEnv): Seq[IRCond] =
    val HasType(cTerm, _) = concl.assertion
    
    val envConds = envIRGenerator.generateConditions(concl.env, codeEnv)
    val termConds = genConclusionTermConds(cTerm, codeEnv)
    termConds ++ envConds

  private def genConclusionTermConds(cTerm: Term, codeEnv: TargetCodeEnv): Seq[IRCond] =
    val constructor = extractTemplate(cTerm)
    val termSubst = constructor.matches(cTerm).get
    val typeSubst = termSubst
      .collect({ case (k, Term.Type(typ)) => k -> typ })
      .toMap
    
    val destructureConds = genConclusionDestructureConds(termSubst, codeEnv)
    val typeConds = genConclusionTypeConds(constructor.types.toSeq, typeSubst, codeEnv)
    destructureConds ++ typeConds

  def genConclusionTypeConds(types: Seq[Type], typeSubst: Map[String, Type], codeEnv: TargetCodeEnv): Seq[IRCond] =
    for
      case t @ Type.Variable(name) <- types 
      if t != Constants.Types.any
    
      typExp = IRNode.Type(IRType.FromCode(codeEnv(t), isOptional = true))

      c <- typeSubst.get(name) match {
        case Some(typ) =>
          typeIRGenerator.generateDestructureDecl(typ, codeEnv, typExp)

        case None =>
          Seq(
            IRCond.TypeDecl(
              declPat = 
                val (_, permanentIdCode) = codeEnv.requestIdentifier(typeIRGenerator.getPermanentTypeVar(t))
                TCP.Var(permanentIdCode.name),
              typExp
            )
          )
      }   
    yield
      c   
  
  def genConclusionDestructureConds(termSubst: Map[String, Term], codeEnv: TargetCodeEnv): Seq[IRCond] =
    for
      case (k, f: Term.Function) <- termSubst.toSeq
      if !f.isGround
    yield
      // Map all args into fresh variables
      val argsAsTemplate = f.args.zipWithIndex.map({
        case (v: Term.Variable, _) => v
        case (_, argIdx) => Term.Variable(s"e${('a' + argIdx).toChar}"): Term.Variable
      })

      val argsAsCode = argsAsTemplate.map(v =>
        val (_, idCode) = codeEnv.requestIdentifier(v)
        idCode
      )
      val declTermArgs = argsAsCode.map(vCode => Term.Variable(vCode.name): Term.Variable)
      
      // Generate a composite term pattern with the fresh args and use it for
      // the destructuring declaration.
      val declTerm = Term.Function(f.name, declTermArgs*)
      
      IRCond.TypeDecl(
        declPat = termIRGenerator.generatePattern(declTerm),
        typExp = IRNode.Type(IRType.FromCode(TCN.Var(k)))
      )

  private def genPremiseConds(premise: Judgement, idx: Int, codeEnv: TargetCodeEnv): Seq[IRCond] =
    val HasType(pTerm, pType) = premise.assertion

    val inductionCall = IRNode.Type(IRType.Induction(
      termIRGenerator.generate(pTerm, codeEnv),
      envIRGenerator.generate(premise.env, codeEnv)
    ))
    typeIRGenerator.generateDestructureDecl(pType, codeEnv, inductionCall)
