package tyes.cli

import java.lang.reflect.Field
import example.*
import tyes.model.Constants
import utils.StringExtensions.*

class LExpressionWithRuntimeTypesParser[T <: tyes.runtime.Type](
  rtTypeEnumClass: Class[T],
  rtTypeObjectClass: Class[_]
) extends AbstractLExpressionWithTypesParser[T]:

  override type TNamedTypeInfo = Field

  override val allNamedTypes = 
    for
      f <- rtTypeObjectClass.getFields
      if f.getType() == rtTypeEnumClass
    yield
      f

  private val rtFunctionTypeClass = 
    rtTypeObjectClass.getClasses
      .find(c => rtTypeEnumClass.isAssignableFrom(c) && c.getSimpleName == Constants.Types.Function.name)
      .map(_.asSubclass(rtTypeEnumClass))

  override val hasFunctionRuntimeType = rtFunctionTypeClass.isDefined

  override def getTypeName(typInfo: Field) = typInfo.getName.decapitalize

  override def getRuntimeType(typInfo: Field) = rtTypeEnumClass.cast(typInfo.get(null))

  override def getFunctionRuntimeType(argTpe: T, retTpe: T) =
    val rtFunTypeCtor = rtFunctionTypeClass.get.getMethod("apply", rtTypeEnumClass, rtTypeEnumClass)
    rtFunTypeCtor.invoke(null, argTpe, retTpe).asInstanceOf[T]
  
  def prettyPrint(typ: T): String = 
    if rtTypeEnumClass.isAssignableFrom(typ.getClass) then
      if typ.getClass.isAnonymousClass then
        return typ.toString.decapitalize
      
      // Special case for functions while they are a special case
      else if typ.getClass.getSimpleName == Constants.Types.Function.name then
        val argTyp = typ.getClass.getMethod("t1").invoke(typ).asInstanceOf[T]
        val retTyp = typ.getClass.getMethod("t2").invoke(typ).asInstanceOf[T]
        
        val argIsFunction = argTyp.getClass.getSimpleName == Constants.Types.Function.name
        return prettyPrintFunctionType(argTyp, retTyp, argIsFunction)

    return s"$typ (raw)"
