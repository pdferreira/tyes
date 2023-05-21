package tyes.compiler

import tyes.compiler.ir.TargetCodeNode
import tyes.model.Term
import tyes.model.Type

val TCN = TargetCodeNode

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
