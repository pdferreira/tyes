package tyes.compiler

import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.model.Label
import tyes.model.terms.TermVariable

class LabelIRGenerator:
  
  def generate(label: Label, codeEnv: TargetCodeEnv = TargetCodeEnv()): TargetCodeNode = label match {
    case Label.Constant(name) => TCN.Text(name)
    case v: Label.Variable => codeEnv(v)
  }

  def generatePattern(label: Label): TargetCodePattern = label match {
    case Label.Constant(name) => TCP.Text(name)
    case Label.Variable(name) => TCP.Var(name)
  }
