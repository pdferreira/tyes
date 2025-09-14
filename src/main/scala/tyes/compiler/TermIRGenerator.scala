package tyes.compiler

import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeTypeRef
import tyes.model.Term
import tyes.model.Type

class TermIRGenerator(
  private val typeIRGenerator: TypeIRGenerator
):
  
  def generate(term: Term, codeEnv: TargetCodeEnv = TargetCodeEnv()): TargetCodeNode = term match {
    case Term.Constant(value: Int) => TCN.Integer(value)
    case Term.Constant(value: String) => TCN.Text(value)
    case Term.Constant(_) => ???
    case v: Term.Variable => codeEnv(v)
    case Term.Function(name, args*) => 
      TCN.ADTConstructorCall(
        TCTypeRef(name),
        args.map(generate(_, codeEnv))*
      )
    case Term.Type(typ) => typeIRGenerator.generate(typ, codeEnv)
  }

  def generatePattern(term: Term): TargetCodePattern = term match {
    case Term.Constant(value: Int) => TCP.Integer(value)
    case Term.Constant(value: String) => TCP.Text(value)
    case Term.Constant(_) => ???
    case Term.Variable(name) => TCP.Var(name)
    case Term.Function(name, args*) => 
      TCP.ADTConstructor(
        TCTypeRef(name),
        args.map(generatePattern)*
      )
    case Term.Type(typ) => typeIRGenerator.generatePattern(typ)
  }
