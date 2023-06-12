package tyes.compiler

import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.IRError

private type TCN = TargetCodeNode
private val TCN = TargetCodeNode

/**
  * Constants and utilities for generated code that uses definitions from tyes.runtime.* 
  */
object RuntimeAPIGenerator:

  private val typeErrorObj = TCN.Var("TypeError")

  def genError(errorIR: IRError[TCN]): TCN = errorIR match {
    case IRError.Generic(messageCode) => TCN.Apply(typeErrorObj, messageCode)
    case IRError.NoType(expCode) => genError("noTypeFor", expCode)
    case IRError.UnexpectedType(obtained, expected) => genError("unexpectedType", obtained, expected)
  }

  private def genError(method: String, args: TCN*) =
    TCN.Apply(TCN.Field(typeErrorObj, method), args*)

  def genCheck(condCode: TCN, errorIR: IRError[TCN]): TCN =
    TCN.Apply(TCN.Var("checkIf"), condCode, genError(errorIR))

  def genCheckEnvSize(envVarCode: TCN, expectedSize: Int): TCN =
    TCN.Apply(TCN.Var("checkEnvSize"), envVarCode, TCN.Integer(expectedSize))

  def genCheckTypeDeclared(typeOptCode: TCN, parentExpCode: TCN): TCN =
    TCN.Apply(TCN.Var("checkTypeDeclared"), typeOptCode, parentExpCode)

  def genExpecting(targetCode: TCN, expectedTypeCode: TCN): TCN =
    // TODO: explore having `expecting` as an extra parameter instead, to allow a better error message 
    TCN.Apply(TCN.Field(targetCode, "expecting"), expectedTypeCode)

  def genExpecting(targetCode: TCN, expectedTypeRef: TCTypeRef): TCN =
    TCN.TypeApply(TCN.Field(targetCode, "expecting"), expectedTypeRef)
