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
