package tyes.compiler

import tyes.compiler.ir.IRError
import tyes.compiler.ir.IRInstr
import tyes.compiler.ir.IRNode
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

  def generateRef(typeName: String): TargetCodeTypeRef =
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
  
  def generateExpectationCheck(typ: Type, codeEnv: TargetCodeEnv, typeProviderCode: TCN): TargetCodeNode = typ match {
    case Type.Named(_) =>
      val typeCode = generate(typ, codeEnv)
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeCode)
    
    case Type.Composite(name, _*) =>
      val typeRef = generateRef(name)
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeRef)

    case _ =>
      // Nothing to check
      typeProviderCode
  }

  def generateDestructureDecl(
    typ: Type, 
    codeEnv: TargetCodeEnv,
    declExpNode: IRNode
  ): Seq[IRInstr] =
    // Save previously bound type variables, before we decl new things in the env
    val previouslyBoundVars = typ
      .typeVariables
      .filter(v => codeEnv.contains(v))
      .toSet[Type]
      
    typ match {
      case _: Type.Named => 
        Seq(IRInstr.Check(declExpNode, resPat = TCP.Any))

      case v: Type.Variable =>
        val permanentV = getPermanentTypeVar(v)
        val (_, realIdCode) = codeEnv.requestIdentifier(permanentV)
        val declPat = TCP.Var(realIdCode.name)

        val declCond = 
          if previouslyBoundVars.contains(permanentV) then
            val expectedTypeNode = generate(permanentV, codeEnv)
            val declTypeNode = realIdCode
            Seq(IRInstr.Cond(
              TCN.Equals(declTypeNode, expectedTypeNode),
              IRError.UnexpectedType(expected = expectedTypeNode, obtained = declTypeNode)
            ))
          else
            Seq.empty

        IRInstr.Check(declExpNode, resPat = declPat) +: declCond
      
      case Type.Composite(tName, args*) =>
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
        // the induction call. Composite patterns can't be destructured directly 
        // due to limitations of the target code, so we split it into two declarations
        val declType = Type.Composite(tName, declTypeArgs*)
        val declPat = generatePattern(declType)
        
        val (_, tmpDeclVarCode) = codeEnv.requestIdentifier(getTempTypeVar(typ))
        val inductionDecls = Seq(
          IRInstr.Check(declExpNode, resPat = TCP.Var(tmpDeclVarCode.name)),
          IRInstr.Check(
            exp = IRNode.Result(tmpDeclVarCode, canFail = false),
            resPat = declPat
          )
        )

        // For all arguments that corresponded to an previously declared type var
        // we need to generate conds that assert the values are the same
        val argTypeReqs = 
          for 
            (origArg, declArgCode) <- args.zip(argsAsCode)
            if previouslyBoundVars.contains(origArg)
          yield
            val expectedTypeNode = generate(origArg, codeEnv)
            val declTypeNode = declArgCode
            IRInstr.Cond(
              TCN.Equals(declTypeNode, expectedTypeNode),
              IRError.UnexpectedType(expected = expectedTypeNode, obtained = declTypeNode)
            )
        
        inductionDecls ++ argTypeReqs
    }
