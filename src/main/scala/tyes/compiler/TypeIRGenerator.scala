package tyes.compiler

import tyes.compiler.ir.TargetCodeDecl
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodeUnit
import tyes.compiler.ir.TargetCodeADTConstructor
import tyes.compiler.ir.TargetCodeBaseTypeCall
import tyes.compiler.ir.TargetCodeTypeRef
import tyes.compiler.Orderings.given
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*
import utils.StringExtensions.*

private val TCN = TargetCodeNode
private val TCD = TargetCodeDecl
private val TCTypeRef = TargetCodeTypeRef
private val TCADTConstructor = TargetCodeADTConstructor

class TypeIRGenerator:

  val typeEnumTypeRef = TCTypeRef("Type")

  def generateEnum(tsDecl: TypeSystemDecl): TargetCodeDecl =
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

  private def generateTypeConstructor(typ: Type): TargetCodeADTConstructor = typ match {
    case Type.Named(name) => TCADTConstructor(name.capitalize)
    case Type.Composite(name, args*) =>
      val argNames = args.map(t => t.asInstanceOf[Type.Variable].name)
      TCADTConstructor(
        name.capitalize,
        params = argNames.map(n => n -> typeEnumTypeRef),
        inherits = Seq(
          TargetCodeBaseTypeCall(typeEnumTypeRef), 
          TargetCodeBaseTypeCall(
            TCTypeRef("tyes", "runtime", "CompositeType"),
            args = argNames.map(TCN.Var.apply)*
          )
        )
      )
    case _ => throw new Exception(s"Not a type constructor: $typ")
  }
