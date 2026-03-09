package tyes.cli

import scala.annotation.targetName
import example.*
import tyes.model.*

abstract class AbstractLExpressionWithTypesParser[T, L] extends LExpressionParser[T]:

  protected type TNamedTypeInfo

  protected val allNamedTypes: Iterable[TNamedTypeInfo]

  protected def getTypeName(typInfo: TNamedTypeInfo): String

  protected def getRuntimeLabel(label: String): L

  protected def getRuntimeType(typInfo: TNamedTypeInfo): T

  protected def getFunctionRuntimeType(argTpe: T, retTpe: T): T

  protected val hasFunctionRuntimeType: Boolean

  protected def getRecordRuntimeType(fields: Seq[(L, T)]): T

  protected val hasRecordRuntimeType: Boolean

  @targetName("prettyPrintType")
  def prettyPrint(typ: T): String

  @targetName("prettyPrintLabel")
  def prettyPrint(label: L): String

  def leafType: Parser[T] = 
    val coreParser =
      allNamedTypes.foldLeft[Parser[T]](failure("Unrecognized type")) { (prevParser, typInfo) =>
        prevParser | (literal(getTypeName(typInfo)) ~> success(getRuntimeType(typInfo)))
      }
      | "(" ~> tpe <~ ")"

    if hasRecordRuntimeType then
      coreParser | recordType
    else
      coreParser

  def functionType: Parser[T] =
    leafType ~ (Constants.Types.Function.operator ~> functionType).? ^^ {
      case argTpe ~ None => argTpe
      case argTpe ~ Some(retTpe) => getFunctionRuntimeType(argTpe, retTpe)
    }
    | "(" ~> leafType <~ ")"

  def recordType =
    import Constants.Types.Record.*
    delimiterL ~> repsep(ident ~ (":" ~> tpe), ",") <~ delimiterR ^^ {
      case fields => getRecordRuntimeType(fields.map({ case f ~ t => (getRuntimeLabel(f), t) }))
    }

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