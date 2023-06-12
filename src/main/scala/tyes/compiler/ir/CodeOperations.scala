package tyes.compiler.ir

trait CodeOperations:
  def negate(code: TargetCodeNode): TargetCodeNode
