package tyes.compiler.ir

import tyes.compiler.target.TargetCodeNode

trait TargetCodeIRGenerator:
  def generate(irNode: IRNode): TargetCodeNode

def canFail(irNode: IRNode): Boolean = irNode match {
  case IRNode.Unexpected => false
  case IRNode.Result(_, canFail) => canFail
  case IRNode.Error(_) => true
  case IRNode.And(conds, next) => conds.exists(c => canFail(c)) || canFail(next)
  case IRNode.Switch(branches, otherwise) => branches.exists((_, n) => canFail(n)) || canFail(otherwise)
  case IRNode.Or(main, alt) => canFail(main) && canFail(alt)
}

def canFail(irInstr: IRInstr): Boolean = irInstr match {
  case IRInstr.Cond(_, _) => true
  case IRInstr.Check(exp, _) => canFail(exp)
}
