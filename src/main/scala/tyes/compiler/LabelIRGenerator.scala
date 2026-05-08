package tyes.compiler

import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.model.Label
import tyes.model.terms.TermVariable

class LabelIRGenerator:
  
  val labelTypeRef = TCTypeRef("Label")

  def generate(label: Label, codeEnv: TargetCodeEnv = TargetCodeEnv()): TargetCodeNode = label match {
    case Label.Constant(name) => TCN.Text(name)
    case v: Label.Variable => codeEnv(v)
  }

  def generatePattern(
    label: Label,
    codeEnv: TargetCodeEnv,
    colVarToMetadata: Map[String, CollectionMetadata]
  ): TargetCodePattern = label match {
    case Label.Constant(name) => TCP.Text(name)
    case v @ Label.Variable(name) =>
      val (_, varCode) = codeEnv.requestIdentifier(v, colMetadata = colVarToMetadata.get(name))
      TCP.Var(varCode.name)
  }
