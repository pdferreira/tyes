package tyes.compiler.ir

trait TargetCodeGenerator:
  def generate(tcNode: TargetCodeNode): String
