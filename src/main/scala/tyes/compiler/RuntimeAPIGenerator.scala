package tyes.compiler

import tyes.compiler.target.TargetCodeNode
import tyes.compiler.ir.IRError

/**
  * Constants and utilities for generated code that uses definitions from tyes.runtime.* 
  */
object RuntimeAPIGenerator:

  private val typeErrorObj = TCN.Var("TypeError")

  def genError(errorIR: IRError): TCN = errorIR match {
    case IRError.Generic(messageCode) => TCN.Apply(typeErrorObj, messageCode)
    case IRError.NoType(expCode) => genError("noTypeFor", expCode)
    case IRError.UnexpectedType(obtained, expected) => genError("unexpectedType", obtained, expected)
    case IRError.UnexpectedEnvSize(envCode, expectedSize) => genError("unexpectedEnvSize", envCode, TCN.Integer(expectedSize))
    case IRError.OneOf(errors*) => genError("oneOf", errors.map(genError)*)
    case IRError.AllOf(errors*) => genError("allOf", errors.map(genError)*)
  }

  private def genError(method: String, args: TCN*) =
    TCN.Apply(TCN.Field(typeErrorObj, method), args*)

  def genCheck(condCode: TCN, errorIR: IRError): TCN =
    TCN.Apply(TCN.Var("checkIf"), condCode, genError(errorIR))

  def genCheckEnvSize(envVarCode: TCN, expectedSize: Int): TCN =
    TCN.Apply(TCN.Var("checkEnvSize"), envVarCode, TCN.Integer(expectedSize))

  def genGetEnvSize(envVarCode: TCN): TCN =
    TCN.Field(envVarCode, "size")

  def genCheckTypeDeclared(typeOptCode: TCN, parentExpCode: TCN): TCN =
    TCN.Apply(TCN.Var("checkTypeDeclared"), typeOptCode, parentExpCode)

  def genExpecting(targetCode: TCN, expectedTypeCode: TCN): TCN =
    // TODO: explore having `expecting` as an extra parameter instead, to allow a better error message 
    TCN.Apply(TCN.Field(targetCode, "expecting"), expectedTypeCode)

  def genExpecting(targetCode: TCN, expectedTypeRef: TCTypeRef): TCN =
    TCN.TypeApply(TCN.Field(targetCode, "expecting"), expectedTypeRef)

  def genTypecheck(expCode: TCN, envCode: TCN): TCN =
    TCN.Apply(TCN.Var("typecheck"), expCode, envCode)

  def genEnvironmentGet(envVarCode: TCN, keyCode: TCN): TCN =
    TCN.Apply(TCN.Field(envVarCode, "get"), keyCode)

  type GenExtractArgsCodeFn = (argsContainer: Seq[TCN] => TCN) => TCN

  def genExtractRangeL(
    expCode: TCN,
    expTypeRef: TCTypeRef,
    genExtractArgsCode: GenExtractArgsCodeFn
  ): (TCTypeRef, TCN) =
    val typeRef = TCTypeRef("Seq", expTypeRef)
    val code = TCN.Apply(
      TCN.Field(expCode, "extractRangeL"),
      genExtractArgsCode(args => TCN.Tuple(args*))
    )
    (typeRef, code)

  def genExtractRangeR(
    expCode: TCN,
    expTypeRef: TCTypeRef, 
    genExtractArgsCode: GenExtractArgsCodeFn
  ): (TCTypeRef, TCN) =
      val typeRef = TCTypeRef("Seq", expTypeRef)
      val code = TCN.Apply(
        TCN.Field(expCode, "extractRangeR"),
        genExtractArgsCode(args => TCN.Tuple(args*))
      )
      (typeRef, code)


  def genExtractRange(
    expCode: TCN,
    expTypeRef: TCTypeRef,
    holeArgIdx: Int,
    genExtractArgsCode: GenExtractArgsCodeFn
  ): (TCTypeRef, TCN) =
    val typeRef = TCTypeRef("Tuple2", TCTypeRef("Seq", expTypeRef), expTypeRef)
    val code = TCN.Apply(
      TCN.Field(expCode, "extractRange"),
      TCN.Integer(holeArgIdx),
      genExtractArgsCode(args => TCN.Apply(TCN.Var("Seq"), args*))
    )
    (typeRef, code)
    
  def genFoldRange(expCode: TCN, initCode: TCN, fCode: TCN): TCN =
    TCN.Apply(TCN.Apply(TCN.Field(expCode, "foldRange"), initCode), fCode)

  def genFoldLeft1(colCode: TCN, funCode: TCN): TCN =
    TCN.Apply(
      TCN.Apply(
        TCN.Field(TCN.Field(colCode, "tail"), "foldLeft"),
        TCN.Field(colCode, "head"),
      ),
      funCode
    )
