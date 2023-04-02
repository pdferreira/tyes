package tyes.compiler.ir

trait IRNodeCompiler[TCode](
  protected val codeOps: CodeOperations[TCode]
):
  def compile(irNode: IRNode[TCode]): TCode


def canFail[TCode](irNode: IRNode[TCode]): Boolean = irNode match {
  case IRNode.Unexpected => false
  case IRNode.Result(_, canFail) => canFail
  case IRNode.Error(_) => true
  case IRNode.And(conds, next) => conds.exists(c => canFail(c)) || canFail(next)
  case IRNode.Switch(branches, otherwise) => branches.exists((_, n) => canFail(n)) || canFail(otherwise)
}

def canFail[TCode](irInstr: IRInstr[TCode]): Boolean = irInstr match {
  case IRInstr.Cond(_, _) => true
  case IRInstr.Decl(_, exp) => canFail(exp) 
}