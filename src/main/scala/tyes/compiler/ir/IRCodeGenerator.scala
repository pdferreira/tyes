package tyes.compiler.ir

trait IRCodeGenerator:
  def generate(cgNode: CodeGenNode): String
