package tyes.compiler.ir.rewrite

import tyes.compiler.ir.IRCond
import tyes.compiler.ir.IRError
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.IRType
import tyes.compiler.ir.IRTypeExpect
import tyes.compiler.target.TargetCodeNode
import utils.collections.*

object NonInductionCondsToSwitchRewrite extends Rewrite[IRNode]:

  private val TCN = TargetCodeNode

  override protected val tryRewrite = {
    case IRNode.And(cs @ _ +: _, n) 
      if cs.forall(c => c match {
        case IRCond.TypeDecl(_, IRNode.Type(_: IRType.FromCode), Some(_)) => true
        case IRCond.TypeDecl(_, _, _) => false
        case _ => true
      })
      =>
        val switchNode = IRNode.Switch(
          branches = Seq(
            cs.foldLeft1(IRCond.And.apply) -> n
          ),
          otherwise =
            val errors = cs.map(condToError)
            if errors.length == 1 then
              IRNode.Error(errors.head)
            else
              IRNode.Error(IRError.AllOf(errors*))
        )
        Some(switchNode)
  }

  private def condToError(cond: IRCond): IRError = cond match {
    case IRCond.EnvSizeIs(envVar, size) => IRError.UnexpectedEnvSize(TCN.Var(envVar), size)
    case IRCond.TypeEquals(t1Code, t2Code) => IRError.UnexpectedType(t1Code, t2Code)
    case IRCond.TermEquals(t1Code, _) => IRError.NoType(t1Code)
    case IRCond.TypeDecl(_, IRNode.Type(irTyp), Some(IRTypeExpect.EqualsTo(expectedTypeCode))) =>
      val typCode = irTyp match {
        case IRType.FromCode(typCode, false) => typCode
      }
      IRError.UnexpectedType(typCode, expectedTypeCode)
  }
