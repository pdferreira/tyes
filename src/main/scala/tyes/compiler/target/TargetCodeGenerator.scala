package tyes.compiler.target

import java.nio.file.Path

trait TargetCodeGenerator:
  def generate(tcUnit: TargetCodeUnit): String
  def generate(tcNode: TargetCodeNode): String
  def getFileName(tcUnit: TargetCodeUnit): Path
