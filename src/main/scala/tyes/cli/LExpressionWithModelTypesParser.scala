package tyes.cli

import example.*
import tyes.model.*
import utils.StringExtensions.*

class LExpressionWithModelTypesParser(types: Set[Type]) extends LExpressionParser[Type]:

  private val allNamedTypes = for case tn @ Type.Named(_) <- types yield tn

  private val hasFunctionType = types.exists({ case Constants.Types.Function(_, _) => true ; case _ => false }) 

  def leafType: Parser[Type] = 
    allNamedTypes.foldLeft[Parser[Type.Named]](failure("Unrecognized type")) { (prevParser, typ) =>
      prevParser | (literal(typ.name) ~> success(typ))
    }
    | "(" ~> tpe <~ ")"

  def functionType: Parser[Type] =
    leafType ~ (Constants.Types.Function.operator ~> functionType).? ^^ {
      case argTpe ~ None => argTpe
      case argTpe ~ Some(retTpe) => Constants.Types.Function(argTpe, retTpe)
    }
    | "(" ~> leafType <~ ")"

  override def tpe: Parser[Type] = 
    if hasFunctionType
    then functionType
    else leafType
    
  def prettyPrint(typ: Type): String = typ match {
    case Type.Named(name) => name
    // Special case for functions while they are a special case
    case Constants.Types.Function(argTyp, retTyp) =>
      val argTypStr = argTyp match {
        case Constants.Types.Function(_, _) => "(" + prettyPrint(argTyp) + ")"
        case _ => prettyPrint(argTyp)
      }
      s"$argTypStr ${Constants.Types.Function.operator} ${prettyPrint(retTyp)}"
    case Type.Composite(name, args*) => name + args.map(prettyPrint).mkString("(", ", ", ")")
    case Type.Variable(name) => s"$name (free)"  
  }
