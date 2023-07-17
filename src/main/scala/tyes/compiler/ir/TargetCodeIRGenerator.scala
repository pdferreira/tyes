package tyes.compiler.ir

import tyes.compiler.target.TargetCodeNode

trait TargetCodeIRGenerator:
  def generate(irNode: IRNode): TargetCodeNode

def canFail(irNode: IRNode): Boolean = irNode match {
  case IRNode.Unexpected => false
  case IRNode.Type(irTyp) => canFail(irTyp)
  case IRNode.Error(_) => true
  case IRNode.And(conds, next) => conds.exists(c => canFail(c)) || canFail(next)
  case IRNode.Switch(branches, otherwise) => branches.exists((_, n) => canFail(n)) || canFail(otherwise)
  case IRNode.Or(main, alt) => canFail(main) && canFail(alt)
}

def canFail(irCond: IRCond): Boolean = irCond match {
  case IRCond.EnvSizeIs(_, _) => true
  case IRCond.TypeEquals(_, _) => true
  case IRCond.TypeDecl(_, _, Some(_)) => true
  case IRCond.TypeDecl(_, typExp, None) => canFail(typExp)
}

def canFail(irType: IRType): Boolean = irType match {
  case IRType.FromCode(_, isOptional) => isOptional
  case IRType.Induction(_, _) => true
  case IRType.EnvGet(_, _) => true
}
