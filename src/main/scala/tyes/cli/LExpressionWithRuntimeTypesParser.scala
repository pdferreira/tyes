package tyes.cli

import example.*
import tyes.model.Constants
import utils.StringExtensions.*

class LExpressionWithRuntimeTypesParser[T <: tyes.runtime.Type](
  rtTypeEnumClass: Class[T],
  rtTypeObjectClass: Class[_]
) extends LExpressionParser[T]:

  private val allNamedTypes = 
    for
      f <- rtTypeObjectClass.getFields
      if f.getType() == rtTypeEnumClass
    yield
      f

  private val rtFunctionTypeClass = 
    rtTypeObjectClass.getClasses
      .find(c => rtTypeEnumClass.isAssignableFrom(c) && c.getSimpleName == Constants.Types.Function.name)
      .map(_.asSubclass(rtTypeEnumClass))
  
  // TODO: refactor commonalities between this class and LExpressionWithModelTypesParser
  def leafType: Parser[T] = 
    allNamedTypes.foldLeft[Parser[T]](failure("Unrecognized type")) { (prevParser, typ) =>
      prevParser | (literal(typ.getName.decapitalize) ~> success(rtTypeEnumClass.cast(typ.get(null))))
    }
    | "(" ~> tpe <~ ")"

  def functionType(rtFunTypeClass: Class[_ <: T]): Parser[T] =
    leafType ~ (Constants.Types.Function.operator ~> functionType(rtFunTypeClass)).? ^^ {
      case argTpe ~ None => argTpe
      case argTpe ~ Some(retTpe) => 
        val rtFunTypeCtor = rtFunTypeClass.getMethod("apply", rtTypeEnumClass, rtTypeEnumClass)
        rtFunTypeCtor.invoke(null, argTpe, retTpe).asInstanceOf[T]
    }
    | "(" ~> leafType <~ ")"

  override def tpe: Parser[T] = rtFunctionTypeClass.fold(leafType)(functionType)

  def prettyPrint(typ: T): String = 
    if rtTypeEnumClass.isAssignableFrom(typ.getClass) then
      if typ.getClass.isAnonymousClass then
        return typ.toString.decapitalize
      
      // Special case for functions while they are a special case
      else if typ.getClass.getSimpleName == Constants.Types.Function.name then
        val argTyp = typ.getClass.getMethod("t1").invoke(typ).asInstanceOf[T]
        val retTyp = typ.getClass.getMethod("t2").invoke(typ).asInstanceOf[T]
        
        val argTypStr =
          if argTyp.getClass.getSimpleName == Constants.Types.Function.name
          then "(" + prettyPrint(argTyp) + ")"
          else prettyPrint(argTyp)
        
        val retTypStr = prettyPrint(retTyp)

        return s"$argTypStr ${Constants.Types.Function.operator} $retTypStr"

    return s"$typ (raw)"
