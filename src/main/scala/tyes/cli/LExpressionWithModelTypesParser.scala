package tyes.cli

import scala.annotation.targetName
import example.*
import tyes.model.*
import utils.StringExtensions.*

object LExpressionWithModelTypesParser:

  private def getAllMatchingTypes[T](types: Iterable[Type])(matches: PartialFunction[Type, T]): Set[T] =
    types.collect({
      case matches(t) => Set(t)
      case Type.Composite(_, args*) => getAllMatchingTypes(args)(matches)
      case _ => Set() 
    }).flatten.toSet

class LExpressionWithModelTypesParser(types: Set[Type]) extends tyes.cli.AbstractLExpressionWithTypesParser[Type, Label]:
  import LExpressionWithModelTypesParser.*

  override type TNamedTypeInfo = Type.Named

  override val allNamedTypes = getAllMatchingTypes(types) {
    case tn @ Type.Named(_) if !Constants.Types.Record.Empty.unapply(tn) => tn
  }

  override val hasFunctionRuntimeType = getAllMatchingTypes(types) {
    case ft @ Constants.Types.Function(_, _) => ft
  }.size > 0

  override val hasRecordRuntimeType: Boolean = getAllMatchingTypes(types) {
    case rt @ Constants.Types.Record(_) => rt
  }.size > 0

  override def getTypeName(tn: Type.Named) = tn.name

  override def getRuntimeType(tn: Type.Named) = tn

  override def getRuntimeLabel(label: String): Label = Label.Constant(label)

  override def getFunctionRuntimeType(argTpe: Type, retTpe: Type) = Constants.Types.Function(argTpe, retTpe)

  override def getRecordRuntimeType(fields: Seq[(Label, Type)]): Type = Constants.Types.Record(fields)

  @targetName("prettyPrintType")
  override def prettyPrint(typ: Type): String = typ match {
    // Special case for functions while they are a special case
    case Constants.Types.Function(argTyp, retTyp) =>
      val argIsFunction = Constants.Types.Function.unapply(argTyp).isDefined
      prettyPrintFunctionType(argTyp, retTyp, argIsFunction)
    // Special case for records while they need a special case
    case Constants.Types.Record(fields) => prettyPrintRecordType(fields)
    case Type.Named(name) => name
    case Type.Label(label) => prettyPrint(label)
    case Type.Composite(name, args*) => name + args.map(prettyPrint).mkString("(", ", ", ")")
    case Type.Variable(name) => s"$name (free)"
  }

  @targetName("prettyPrintLabel")
  override def prettyPrint(label: Label): String = label match {
    case Label.Constant(name) => name
    case Label.Variable(name) => s"$name (free)"
  }
