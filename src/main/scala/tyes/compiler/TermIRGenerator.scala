package tyes.compiler

import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodePattern
import tyes.compiler.ir.TargetCodeTypeRef
import tyes.model.Term
import tyes.model.Type

private val TCN = TargetCodeNode
private val TCP = TargetCodePattern
private val TCTypeRef = TargetCodeTypeRef

class TermIRGenerator(
  private val typeIRGenerator: TypeIRGenerator
):
  
  def generate(term: Term, codeEnv: TargetCodeEnv = TargetCodeEnv()): TargetCodeNode = term match {
    case Term.Constant(value: Int) => TCN.Integer(value)
    case Term.Constant(value: String) => TCN.Text(value)
    case Term.Variable(name) => codeEnv.get(name).getOrElse(TCN.Var(name))
    case Term.Function(name, args*) => 
      TCN.Apply(
        TCN.Var(name),
        args.map(generate(_, codeEnv))*
      )
    case Term.Type(typ) => typeIRGenerator.generate(typ, codeEnv)
  }

  def generatePattern(term: Term): TargetCodePattern = term match {
    case Term.Constant(_) => ???
    case Term.Variable(name) => TCP.Var(name)
    case Term.Function(name, args*) => 
      TCP.ADTConstructor(
        TCTypeRef(name),
        args.map(generatePattern)*
      )
    case Term.Type(typ) => typeIRGenerator.generatePattern(typ)
  }
