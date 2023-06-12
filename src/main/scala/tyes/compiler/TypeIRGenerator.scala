package tyes.compiler

import tyes.compiler.ir.TargetCodeADTConstructor
import tyes.compiler.ir.TargetCodeDecl
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodePattern
import tyes.compiler.ir.TargetCodeTypeRef
import tyes.compiler.ir.TargetCodeUnit
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
    case Type.Variable(name) => codeEnv.get(name).getOrElse(TCN.Var(name))
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
