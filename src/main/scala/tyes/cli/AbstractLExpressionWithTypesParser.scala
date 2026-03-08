package tyes.cli

import scala.annotation.targetName
import example.*
import tyes.model.*

abstract class AbstractLExpressionWithTypesParser[T, L] extends LExpressionParser[T]:

  protected type TNamedTypeInfo

  protected val allNamedTypes: Seq[TNamedTypeInfo]

  protected def getTypeName(typInfo: TNamedTypeInfo): String

  protected def getRuntimeType(typInfo: TNamedTypeInfo): T

  protected def getFunctionRuntimeType(argTpe: T, retTpe: T): T

  protected val hasFunctionRuntimeType: Boolean

  @targetName("prettyPrintType")
  def prettyPrint(typ: T): String

  @targetName("prettyPrintLabel")
  def prettyPrint(label: L): String

  def leafType: Parser[T] = 
    allNamedTypes.foldLeft[Parser[T]](failure("Unrecognized type")) { (prevParser, typInfo) =>
      prevParser | (literal(getTypeName(typInfo)) ~> success(getRuntimeType(typInfo)))
    }
    | "(" ~> tpe <~ ")"

  def functionType: Parser[T] =
    leafType ~ (Constants.Types.Function.operator ~> functionType).? ^^ {
      case argTpe ~ None => argTpe
      case argTpe ~ Some(retTpe) => getFunctionRuntimeType(argTpe, retTpe)
    }
    | "(" ~> leafType <~ ")"

  override def tpe: Parser[T] = 
    if hasFunctionRuntimeType
    then functionType
    else leafType
  
  protected def prettyPrintFunctionType(argTyp: T, retTyp: T, argTypIsFunction: Boolean) =
    val argTypStr =
      if argTypIsFunction
      then s"(${prettyPrint(argTyp)})"
      else prettyPrint(argTyp)

    s"$argTypStr ${Constants.Types.Function.operator} ${prettyPrint(retTyp)}"

  protected def prettyPrintRecordType(fields: Seq[(L, T)]) =
    import Constants.Types.Record.*
    fields.map((f, t) => prettyPrint(f) + ": " + prettyPrint(t)).mkString(delimiterL, ", ", delimiterR)