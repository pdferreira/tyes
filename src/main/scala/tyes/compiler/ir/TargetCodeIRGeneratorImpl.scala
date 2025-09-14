package tyes.compiler.ir

import tyes.compiler.target.TargetCodeForCursor
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeNodeOperations.*
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeTypeRef
import tyes.compiler.RuntimeAPIGenerator
import utils.collections.*

private val TCFC = TargetCodeForCursor
private val TCN = TargetCodeNode
private val TCP = TargetCodePattern
private val TCTypeRef = TargetCodeTypeRef

class TargetCodeIRGeneratorImpl(
  private val expVar: TCN.Var
) extends TargetCodeIRGenerator:

  def generate(irNode: IRNode): TargetCodeNode = generate(irNode, eitherIsExpected = true)
    
  private def generate(irNode: IRNode, eitherIsExpected: Boolean): TargetCodeNode = irNode match {
    case IRNode.Unexpected => 
      TCN.Throw(TargetCodeTypeRef("Exception"), TCN.Text("unexpected"))
    
    case IRNode.Error(err) =>
      assert(eitherIsExpected, "Returning Either must be expected in Error")
      RuntimeAPIGenerator.genError(err)

    case IRNode.Type(irType) => generate(irType, eitherIsExpected)
    
    case IRNode.And(IRCond.TypeDecl(TCP.Var(resVar), typExp, None) :: Nil, IRNode.Type(IRType.FromCode(TCN.Var(resVar2), /*isOptional*/false))) 
      if resVar == resVar2 && canFail(typExp)  
    =>
      generate(typExp, eitherIsExpected)
    
    case IRNode.And(cs :+ IRCond.TypeDecl(TCP.Var(resVar), typExp, None), IRNode.Type(IRType.FromCode(TCN.Var(resVar2), isOptional)))
      if resVar == resVar2 && canFail(typExp) == isOptional
    =>
      generate(IRNode.And(cs, typExp), eitherIsExpected)
    
    case IRNode.And(conds, next) =>
      assert(eitherIsExpected, "Returning Either must be expected in Ands")
      TCN.For(
        cursors = conds.map(generateCursor), 
        body = generate(next, eitherIsExpected = false)
      )
    
    case IRNode.Switch(branches, otherwise) =>
      val failureIsPossible = branches.exists((_, n) => canFail(n)) || canFail(otherwise)
      val otherwiseNode = generate(otherwise, eitherIsExpected || failureIsPossible)

      branches.foldRight(otherwiseNode) { case ((cond, next), elseNode) =>
        TCN.If(
          generateBool(cond),
          generate(next, eitherIsExpected || failureIsPossible),
          elseNode
        )
      }
    
    case IRNode.Or(main, _) if !canFail(main) => generate(main, eitherIsExpected)
    
    case IRNode.Or(main, alt) =>
      val mainNode = generate(main, eitherIsExpected)
      val altNode = generate(alt, eitherIsExpected = true)
      TCN.Apply(
        TCN.Field(
          mainNode,
          "orElse"
        ),
        altNode
      )
  }

  private def generate(irType: IRType, eitherIsExpected: Boolean): TargetCodeNode = irType match {
    case IRType.FromCode(typCode, /*isOptional*/true) =>
      if eitherIsExpected
      then RuntimeAPIGenerator.genCheckTypeDeclared(typCode, expVar)
      else ???

    case IRType.FromCode(typCode, /*isOptional*/false) =>
      if eitherIsExpected
      then wrapAsRight(typCode)
      else typCode

    case IRType.Induction(expCode, envCode) =>
      if eitherIsExpected
      then RuntimeAPIGenerator.genTypecheck(expCode, envCode)
      else ???

    case IRType.EnvGet(envVar, keyCode) =>
      if eitherIsExpected
      then RuntimeAPIGenerator.genEnvironmentGet(TCN.Var(envVar), keyCode)
      else ???
  }

  private def generateCursor(irCond: IRCond): TargetCodeForCursor = irCond match {
    case IRCond.EnvSizeIs(envVar, size) => 
      TCFC.Iterate(TCP.Any, RuntimeAPIGenerator.genCheckEnvSize(TCN.Var(envVar), size))

    case IRCond.TypeEquals(t1Code, t2Code) =>
      TCFC.Iterate(
        TCP.Any,
        RuntimeAPIGenerator.genExpecting(t1Code, t2Code)
      )

    case IRCond.TermEquals(t1Code, t2Code) =>
      TCFC.Iterate(
        TCP.Any,
        RuntimeAPIGenerator.genCheck(TCN.Equals(t1Code, t2Code), IRError.NoType(t1Code))
      )

    case IRCond.OfType(termCode, typRef) =>
      TCFC.Iterate(
        TCP.Any,
        RuntimeAPIGenerator.genExpecting(termCode, typRef)
      )

    case IRCond.TypeDecl(declPat, typExp, None) if !canFail(typExp) =>
      TCFC.Let(declPat, generate(typExp, eitherIsExpected = false))
      
    case IRCond.TypeDecl(declPat, typExp, expectOpt) =>
      val typExpCode = generate(typExp, eitherIsExpected = true)
      TCFC.Iterate(
        declPat, 
        expectOpt match {
          case None => typExpCode
          case Some(irExpect) => genExpectationCheck(typExpCode, irExpect)
        }
      )
  
    case _: IRCond.And => ???
  }

  private def genExpectationCheck(
    typeProviderCode: TargetCodeNode,
    expectation: IRTypeExpect
  ): TargetCodeNode = expectation match {
    case IRTypeExpect.OfType(typeRef) =>
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeRef)
    
    case IRTypeExpect.EqualsTo(typeCode) =>
      RuntimeAPIGenerator.genExpecting(typeProviderCode, typeCode)
  }

  private def generateBool(irCond: IRCond): TargetCodeNode = irCond match {
    case IRCond.EnvSizeIs(envVar, size) =>  
      TCN.Equals(
        RuntimeAPIGenerator.genGetEnvSize(TCN.Var(envVar)),
        TCN.Integer(size)
      )

    case IRCond.TypeEquals(t1Code, t2Code) =>
      TCN.Equals(t1Code, t2Code)

    case IRCond.TermEquals(t1Code, t2Code) =>
      TCN.Equals(t1Code, t2Code)

    case IRCond.And(left, right) =>
      TCN.And(generateBool(left), generateBool(right))

    case IRCond.OfType(termCode, typRef) =>
      TCN.TypeCheck(termCode, typRef)

    case IRCond.TypeDecl(_, _, _) => ???

  }

  private def wrapAsRight(value: TargetCodeNode): TargetCodeNode = TCN.Apply(TCN.Var("Right"), value)
