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
import tyes.model.indexes.*
import tyes.model.TyesLanguageExtensions.*
import utils.StringExtensions.*

class TypeIRGenerator(labelIRGenerator: LabelIRGenerator):

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
    case Type.Label(_) => Set()
    case Type.Variable(_) => Set()
    case Type.Composite(name, args*) => 
      val self = Type.Composite(
        name,
        args.zipWithIndex.map((arg, idx) => arg match {
          case _: Type.Label => Type.Label(Label.Variable(s"l${idx + 1}"))
          case _ => Type.Variable(s"t${idx + 1}")
        })*
      )
      Set(self) ++ inferTypeConstructors(args)
  }).toSet

  private def getTypeNameInCode(name: String) = name.capitalize

  private def generateTypeConstructor(typ: Type): TargetCodeADTConstructor = typ match {
    case Type.Named(name) => TCADTConstructor(getTypeNameInCode(name))
    case Type.Composite(name, args*) =>
      val params = args.map(t => t match {
        case Type.Variable(name) => name -> typeEnumTypeRef
        case Type.Label(Label.Variable(name)) => name -> labelIRGenerator.labelTypeRef
        case _ => throw new IllegalArgumentException(t.getClass.getSimpleName)
      })
      TCADTConstructor(getTypeNameInCode(name), params = params)
    case _ => throw new Exception(s"Not a type constructor: $typ")
  }

  def generate(typ: Type, codeEnv: TargetCodeEnv = TargetCodeEnv()): TargetCodeNode = typ match {
    case Constants.Types.any => TCN.Var("_")
    case Type.Named(name) => TCN.ADTConstructorCall(generateRef(name))
    case Type.Label(label) => labelIRGenerator.generate(label, codeEnv)
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
    case Type.Label(label) => labelIRGenerator.generatePattern(label)
    case Type.Variable(name) => TCP.Var(name)
    case Type.Composite(name, args*) => 
      var typeArgs = args.map(generatePattern)
      TCP.ADTConstructor(generateRef(name), typeArgs*)
  }

  def getTempTypeVar(typ: Type, nonVarSuffix: String = ""): Type.Variable =
    if typ == Constants.Types.any then
      Constants.Types.any
    else
      val varName = typ match {
        case Type.Variable(name) => name
        case Type.Named(_) => "ct" + nonVarSuffix
        case tc @ Type.Composite(name, _*) =>
          val prefix = name.filter(_.isUpper).toLowerCase
          val cursors = tc.variables
            .collect(extractIndex.unlift)
            .map(_._2)
            .filter(_.toIntOption.isEmpty)
            .toSet
          assert(cursors.size <= 1, s"Assumption: at most single cursor used in all variables, found: ${cursors.mkString(", ")}")
          if cursors.size == 1 then
            indexedVar(prefix + nonVarSuffix, cursors.head)
          else
            prefix + nonVarSuffix
      }
      Type.Variable("_" + varName)

  def getPermanentTypeVar(tempVar: Type.Variable): Type.Variable =
    if tempVar == Constants.Types.any then
      tempVar
    else
      Type.Variable(getPermanentTypeVarName(tempVar.name))
      
  def getPermanentTypeVarName(name: String): String =
    if name.startsWith("_") then
      name.substring(1)
    else
      name
  
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
    val previouslyBoundVars = typ.typeVariables.concat(typ.labelVariables: Iterable[terms.TermVariable])
      .filter(v => codeEnv.contains(v))
      .toSet

    def isPreviouslyBoundVar(typ: Type): Boolean = typ match {
      case v: Type.Variable => previouslyBoundVars.contains(v)
      case Type.Label(v: Label.Variable) => previouslyBoundVars.contains(v)
      case _ => false
    }
      
    typ match {
      case _: Type.Named => 
        Seq(IRCond.TypeDecl(TCP.Any, declTypeExp, genExpectationCheck(typ, codeEnv)))

      case v: Type.Variable =>
        val permanentV = getPermanentTypeVar(v)
        val expectationCheck = genExpectationCheck(permanentV, codeEnv)
        val declPat =
          if isPreviouslyBoundVar(permanentV) then
            TCP.Any
          else
            val (_, realIdCode) = codeEnv.requestIdentifier(permanentV)
            TCP.Var(realIdCode.name)

        Seq(IRCond.TypeDecl(declPat, declTypeExp, expectationCheck))
      
      case Type.Composite(tName, args*) =>
        if args.forall(a => isPreviouslyBoundVar(a)) then
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
          val argsAsTemplate: Seq[Type.Variable | Label.Variable] = args.zipWithIndex.map({
            case (v: Type.Variable, _) => v
            case (Type.Label(l @ Label.Variable(_)), _) => l
            case (_, argIdx) => Type.Variable(s"t${('a' + argIdx).toChar}")
          })

          val argTemplatesToCode = argsAsTemplate.map(v =>
            val (_, idCode) = codeEnv.requestIdentifier(v)
            (v, idCode)
          )
          val declTypeArgs = argTemplatesToCode.map((v, vCode) => v match {
            case _: Type.Variable => Type.Variable(vCode.name)
            case _: Label.Variable => Type.Label(Label.Variable(vCode.name))
          })
          
          // Generate a composite type pattern with the fresh args and use it for
          // the induction call.
          val declType = Type.Composite(tName, declTypeArgs*)
          val declPat = generatePattern(declType)
          
          val inductionDecl = IRCond.TypeDecl(declPat, declTypeExp, expectationCheck)

          // For all arguments that corresponded to an previously declared type var
          // we need to generate conds that assert the values are the same
          val argTypeReqs = 
            for 
              (origArg, declArgCode) <- args.zip(argTemplatesToCode.unzip._2)
              if isPreviouslyBoundVar(origArg)
            yield
              val expectedTypeNode = generate(origArg, codeEnv)
              val declTypeNode = declArgCode
              IRCond.TypeEquals(declTypeNode, expectedTypeNode)

          inductionDecl +: argTypeReqs
    }
