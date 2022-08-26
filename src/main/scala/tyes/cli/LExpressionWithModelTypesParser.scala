package tyes.cli

import example.*
import tyes.model.*
import utils.StringExtensions.*

class LExpressionWithModelTypesParser(types: Set[Type]) extends tyes.cli.AbstractLExpressionWithTypesParser[Type]:

  override type TNamedTypeInfo = Type.Named

  override val allNamedTypes = Seq.from(for case tn @ Type.Named(_) <- types yield tn)

  override val hasFunctionRuntimeType = types.exists({ case Constants.Types.Function(_, _) => true ; case _ => false })

  override def getTypeName(tn: Type.Named) = tn.name

  override def getRuntimeType(tn: Type.Named) = tn 

  override def getFunctionRuntimeType(argTpe: Type, retTpe: Type) = Constants.Types.Function(argTpe, retTpe)
    
  override def prettyPrint(typ: Type): String = typ match {
    case Type.Named(name) => name
    // Special case for functions while they are a special case
    case Constants.Types.Function(argTyp, retTyp) =>
      val argIsFunction = Constants.Types.Function.unapply(argTyp).isDefined
      prettyPrintFunctionType(argTyp, retTyp, argIsFunction)
    case Type.Composite(name, args*) => name + args.map(prettyPrint).mkString("(", ", ", ")")
    case Type.Variable(name) => s"$name (free)"
  }
