package tyes.cli

import java.lang.reflect.Method
import example.*
import tyes.model.Constants
import utils.StringExtensions.*

class LExpressionWithRuntimeTypesParser[T <: tyes.runtime.Type](
  rtTypeEnumClass: Class[T],
  rtTypeObjectClass: Class[_],
  rtTypeObject: Object
) extends AbstractLExpressionWithTypesParser[T]:

  override type TNamedTypeInfo = Method

  override val allNamedTypes = 
    for
      m <- rtTypeObjectClass.getMethods()
      if m.getParameterCount() == 0 && m.getReturnType() == rtTypeEnumClass
    yield
      m

  private val rtFunctionTypeObject = 
    rtTypeObjectClass
      .getMethods
      .find(m => m.getName == Constants.Types.Function.name)
      .map(_.invoke(rtTypeObject))

  override val hasFunctionRuntimeType = rtFunctionTypeObject.isDefined

  override def getTypeName(typInfo: Method) = typInfo.getName.decapitalize

  override def getRuntimeType(typInfo: Method) = rtTypeEnumClass.cast(typInfo.invoke(rtTypeObject))

  override def getFunctionRuntimeType(argTpe: T, retTpe: T) =
    val rtFunTypeObj = rtFunctionTypeObject.get
    val rtFunTypeCtor = rtFunTypeObj.getClass.getMethod("apply", rtTypeEnumClass, rtTypeEnumClass)
    rtFunTypeCtor.invoke(rtFunTypeObj, argTpe, retTpe).asInstanceOf[T]
  
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
