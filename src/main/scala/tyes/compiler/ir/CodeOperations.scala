package tyes.compiler.ir

trait CodeOperations[TCode]:
  def negate(code: TCode): TCode
