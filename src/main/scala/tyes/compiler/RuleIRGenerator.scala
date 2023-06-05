package tyes.compiler

import tyes.compiler.ir.IRInstr
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRError
import tyes.compiler.ir.TargetCodeNode
import tyes.model.*
import utils.collections.*

private val TCN = TargetCodeNode

class RuleIRGenerator(
  private val typeIRGenerator: TypeIRGenerator,
  private val termIRGenerator: TermIRGenerator
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
          case Term.Type(typ) => Term.Type(typ match {
            case Type.Variable(_) => typ
            case Type.Named(_) => Type.Variable("ct" + getSuffix(idx))
            case Type.Composite(_, _*) => Type.Variable("ct" + getSuffix(idx))
          })
        }
      }
      Term.Function(fnName, argsAsVariables*)
    case _ => term
  }

  case class GenerateOutput(node: IRNode[TargetCodeNode], condition: Option[TargetCodeNode])

  def generate(rule: RuleDecl, parentCodeEnv: TargetCodeEnv, overallTemplate: Term): GenerateOutput =
    // TODO: this is pretty much the POC code verbatim (for single-rule handling) and needs to be properly split
    val HasType(cTerm, cType) = rule.conclusion.assertion

    val codeEnv = new TargetCodeEnv(Some(parentCodeEnv))

    // Pre-register the simple type names for the premises 
    for case Judgement(_, HasType(_, Type.Variable(name))) <- rule.premises do
      codeEnv.requestIdentifier(name)

    val premiseConds = rule.premises
      .zipWithIndex
      .flatMap((p, idx) => genPremiseConds(p, idx, codeEnv))

    var result = IRNode.Result(typeIRGenerator.generate(cType, codeEnv), canFail = false)
    
    if !premiseConds.isEmpty then
      result = IRNode.And(
        conds = premiseConds,
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

    for (k, v) <- constructorReqs
      yield TCN.Equals(TCN.Var(k), termIRGenerator.generate(v))

  private def genPremiseConds(premise: Judgement, idx: Int, codeEnv: TargetCodeEnv): Seq[IRInstr[TargetCodeNode]] =
    val HasType(pTerm, pType) = premise.assertion
    pType match {
      case Type.Variable(name) =>
        val (declInstr, _) = genInductionCall(name, pTerm, codeEnv) 
        Seq(declInstr)

      case Type.Named(name) =>
        val pTypeCode = typeIRGenerator.generate(pType)
        // TODO: explore having `expecting` as an extra parameter instead, to allow a better error message 
        val (declInstr, _) = genInductionCall("t" + (idx + 1), pTerm, codeEnv, indCall => 
          TCN.Apply(TCN.Field(indCall, "expecting"), pTypeCode)
        )
        Seq(declInstr)

      case Type.Composite(tName, args*) =>
        val (innerResTDeclInstr, innerResTDeclVarCode) = genInductionCall("_t" + (idx + 1), pTerm, codeEnv)
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
          IRInstr.Decl(
            resTId,
            IRNode.Result(
              TCN.Apply(
                TCN.Var("cast[Type.$FunType]"), // small hack, just for poc's sake
                innerResTDeclVarCode,
                TCN.FormattedText("expected ", termIRGenerator.generate(pTerm, codeEnv), " to have type ")
              ),
              canFail = true
            )
          )
        )
        
        inductionDecls ++ argTypeReqs
    }

  private def genInductionCall(
    declVar: String,
    inductionTerm: Term,
    codeEnv: TargetCodeEnv,
    transformCall: TargetCodeNode => TargetCodeNode = tcn => tcn 
  ): (IRInstr[TargetCodeNode], TargetCodeNode) =
    val inductionTermCall = termIRGenerator.generate(inductionTerm, codeEnv)
    val (realDeclVar, realDeclVarCode) = codeEnv.requestIdentifier(declVar)
    val instr = IRInstr.Decl(
      realDeclVar,
      IRNode.Result(
        transformCall(TCN.Apply(
          TCN.Var("typecheck"),
          inductionTermCall,
          TCN.Var("env")
        )), 
        canFail = true
      )
    )
    (instr, realDeclVarCode)
