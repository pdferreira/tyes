package tyes.cli

import java.lang.reflect.Method
import scala.annotation.targetName
import scala.collection.immutable.ArraySeq
import example.*
import tyes.model.Constants
import utils.StringExtensions.*

class LExpressionWithRuntimeTypesParser[T <: tyes.runtime.Type](
  rtTypeEnumClass: Class[T],
  rtTypeObjectClass: Class[?],
  rtTypeObject: Object
) extends AbstractLExpressionWithTypesParser[T, tyes.runtime.Label]:

  override type TNamedTypeInfo = Method

  override val allNamedTypes = 
    for
      m <- ArraySeq.unsafeWrapArray(rtTypeObjectClass.getMethods())
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
  
  private def tryGetRecordRuntimeTypeFields(typ: T): Option[Seq[(tyes.runtime.Label, T)]] =
    if typ.getClass.isAnonymousClass then
      if typ.toString == Constants.Types.Record.Empty.name then
        return Some(Seq())
      else
        return None
    else if typ.getClass.getSimpleName == Constants.Types.Record.name then
      val label = typ.getClass.getMethod("l1").invoke(typ).asInstanceOf[tyes.runtime.Label]
      val expTyp = typ.getClass.getMethod("t2").invoke(typ).asInstanceOf[T]
      val restTyp = typ.getClass.getMethod("t3").invoke(typ).asInstanceOf[T]
      return tryGetRecordRuntimeTypeFields(restTyp).map((label, expTyp) +: _)
    else
      return None

  @targetName("prettyPrintType")
  override def prettyPrint(typ: T): String = 
    if rtTypeEnumClass.isAssignableFrom(typ.getClass) then
      // Special case for functions while they are a special case
      if typ.getClass.getSimpleName == Constants.Types.Function.name then
        val argTyp = typ.getClass.getMethod("t1").invoke(typ).asInstanceOf[T]
        val retTyp = typ.getClass.getMethod("t2").invoke(typ).asInstanceOf[T]
        
        val argIsFunction = argTyp.getClass.getSimpleName == Constants.Types.Function.name
        return prettyPrintFunctionType(argTyp, retTyp, argIsFunction)

      // Special case for records while they are a special case
      val fields = tryGetRecordRuntimeTypeFields(typ)
      if fields.isDefined then
        return prettyPrintRecordType(fields.get)

      else if typ.getClass.isAnonymousClass then
        return typ.toString.decapitalize

    return s"$typ (raw)"

  @targetName("prettyPrintLabel")
  override def prettyPrint(label: tyes.runtime.Label): String = label
