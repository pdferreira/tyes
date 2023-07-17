package tyes.compiler

import tyes.compiler.ir.IRCond
import tyes.compiler.ir.IRError
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRType
import tyes.compiler.ir.IRTypeExpect
import tyes.compiler.target.TargetCodeADTConstructor
import tyes.compiler.target.TargetCodeDecl
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.target.TargetCodeUnit
import tyes.compiler.Orderings.given
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*
import utils.StringExtensions.*

private val TCD = TargetCodeDecl
private val TCN = TargetCodeNode
private val TCP = TargetCodePattern
private val TCTypeRef = TargetCodeTypeRef
private val TCADTConstructor = TargetCodeADTConstructor

class TypeIRGenerator:

  val typeEnumTypeRef = TCTypeRef("Type")

  def generateDecl(tsDecl: TypeSystemDecl): TargetCodeDecl =
    val typeConstructors = inferTypeConstructors(tsDecl.types).toSeq
    TCD.ADT(
      name = typeEnumTypeRef.name,
      inherits = Seq(
        TCTypeRef("tyes", "runtime", "Type")
      ),
      constructors = typeConstructors.sorted.map(generateTypeConstructor)
    )

  private def inferTypeConstructors(types: Iterable[Type]): Set[Type] = types.flatMap({
    case tn @ Type.Named(_) => Set(tn)
    case Type.Variable(_) => Set()
    case Type.Composite(name, args*) => 
      val self = Type.Composite(name, args.indices.map(idx => Type.Variable(s"t${idx + 1}"))*)
      Set(self) ++ inferTypeConstructors(args)
  }).toSet

  private def getTypeNameInCode(name: String) = name.capitalize

  private def generateTypeConstructor(typ: Type): TargetCodeADTConstructor = typ match {
    case Type.Named(name) => TCADTConstructor(getTypeNameInCode(name))
    case Type.Composite(name, args*) =>
      val argNames = args.map(t => t.asInstanceOf[Type.Variable].name)
      TCADTConstructor(
        getTypeNameInCode(name),
        params = argNames.map(n => n -> typeEnumTypeRef),
        inherits = Seq(
          TCN.ADTConstructorCall(typeEnumTypeRef), 
          TCN.ADTConstructorCall(
            TCTypeRef("tyes", "runtime", "CompositeType"),
            args = argNames.map(TCN.Var.apply)*
          )
        )
      )
    case _ => throw new Exception(s"Not a type constructor: $typ")
  }

  def generate(typ: Type, codeEnv: TargetCodeEnv = TargetCodeEnv()): TargetCodeNode = typ match {
    case Constants.Types.any => TCN.Var("_")
    case Type.Named(name) => TCN.ADTConstructorCall(generateRef(name))
    case v: Type.Variable => codeEnv(v)
    case Type.Composite(name, args*) => 
      var typeArgs = args.map(generate(_, codeEnv))
      TCN.ADTConstructorCall(generateRef(name), typeArgs*)
  }

  private def generateRef(typeName: String): TargetCodeTypeRef =
    TCTypeRef(typeEnumTypeRef.name, getTypeNameInCode(typeName))

  def generatePattern(typ: Type): TargetCodePattern = typ match {
    case Constants.Types.any => TCP.Any
    case Type.Named(name) => TCP.ADTConstructor(generateRef(name))
    case Type.Variable(name) => TCP.Var(name)
    case Type.Composite(name, args*) => 
      var typeArgs = args.map(generatePattern)
      TCP.ADTConstructor(generateRef(name), typeArgs*)
  }

  def getTempTypeVar(typ: Type, nonVarSuffix: String = ""): Type.Variable =
    if typ == Constants.Types.any then
      Constants.Types.any
    else
      val prefix = typ match {
        case Type.Variable(name) => name
        case Type.Named(_) => "ct"
        case Type.Composite(name, _*) => name.filter(_.isUpper).toLowerCase
      }
      val suffix = typ match {
        case Type.Variable(_) => ""
        case _ => nonVarSuffix
      }
      Type.Variable("_" + prefix + suffix)

  def getPermanentTypeVar(tempVar: Type.Variable): Type.Variable =
    if tempVar == Constants.Types.any then
      tempVar
    else
      val name = tempVar.name
      if name.startsWith("_") then
        Type.Variable(name.substring(1))
      else
        tempVar
  
  private def genExpectationCheck(
    typ: Type,
    codeEnv: TargetCodeEnv,
    checkArgs: Boolean = false
  ): Option[IRTypeExpect] = typ match {
    case v: Type.Variable =>
      if codeEnv.contains(v) then
        val typeCode = generate(v, codeEnv)
        Some(IRTypeExpect.EqualsTo(typeCode))
      else
        None

    case Type.Composite(name, _*) if !checkArgs =>
      val typeRef = generateRef(name)
      Some(IRTypeExpect.OfType(typeRef))

    case _ =>
      val typeCode = generate(typ, codeEnv)
      Some(IRTypeExpect.EqualsTo(typeCode))
  }

  def generateDestructureDecl(
    typ: Type, 
    codeEnv: TargetCodeEnv,
    declTypeExp: IRNode
  ): Seq[IRCond] =
    // Save previously bound type variables, before we decl new things in the env
    val previouslyBoundVars = typ
      .typeVariables
      .filter(v => codeEnv.contains(v))
      .toSet[Type]
      
    typ match {
      case _: Type.Named => 
        Seq(IRCond.TypeDecl(TCP.Any, declTypeExp, genExpectationCheck(typ, codeEnv)))

      case v: Type.Variable =>
        val permanentV = getPermanentTypeVar(v)
        val expectationCheck = genExpectationCheck(permanentV, codeEnv)
        val declPat =
          if previouslyBoundVars.contains(permanentV) then
            TCP.Any
          else
            val (_, realIdCode) = codeEnv.requestIdentifier(permanentV)
            TCP.Var(realIdCode.name)

        Seq(IRCond.TypeDecl(declPat, declTypeExp, expectationCheck))
      
      case Type.Composite(tName, args*) =>
        if args.forall(a => previouslyBoundVars.contains(a)) then
          // If all the type arguments correspond to already bound vars
          // we can assert a more precise expected type
          Seq(IRCond.TypeDecl(
            TCP.Any,
            declTypeExp,
            genExpectationCheck(typ, codeEnv, checkArgs = true)
          ))
        else
          val expectationCheck = genExpectationCheck(typ, codeEnv)

          // Map all args into fresh variables
          val argsAsTemplate = args.zipWithIndex.map({
            case (v: Type.Variable, _) => v
            case (_, argIdx) => Type.Variable(s"t${('a' + argIdx).toChar}"): Type.Variable
          })

          val argsAsCode = argsAsTemplate.map(v =>
            val (_, idCode) = codeEnv.requestIdentifier(v)
            idCode
          )
          val declTypeArgs = argsAsCode.map(vCode => Type.Variable(vCode.name): Type.Variable)
          
          // Generate a composite type pattern with the fresh args and use it for
          // the induction call.
          val declType = Type.Composite(tName, declTypeArgs*)
          val declPat = generatePattern(declType)
          
          val inductionDecl = IRCond.TypeDecl(declPat, declTypeExp, expectationCheck)

          // For all arguments that corresponded to an previously declared type var
          // we need to generate conds that assert the values are the same
          val argTypeReqs = 
            for 
              (origArg, declArgCode) <- args.zip(argsAsCode)
              if previouslyBoundVars.contains(origArg)
            yield
              val expectedTypeNode = generate(origArg, codeEnv)
              val declTypeNode = declArgCode
              IRCond.TypeEquals(declTypeNode, expectedTypeNode)

          inductionDecl +: argTypeReqs
    }
