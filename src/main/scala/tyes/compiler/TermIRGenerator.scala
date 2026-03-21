package tyes.compiler

import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodePattern
import tyes.compiler.target.TargetCodeTypeRef
import tyes.model.Term
import tyes.model.TyesLanguageExtensions.*

class TermIRGenerator(
  private val typeIRGenerator: TypeIRGenerator,
  private val labelIRGenerator: LabelIRGenerator,
  private val rangeIRGenerator: RangeIRGenerator,
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
    case Term.Label(label) => labelIRGenerator.generate(label, codeEnv)
    case r: Term.Range =>
      r.toConcrete(Term.Function(_, _*)).map(generate(_, codeEnv)).getOrElse {
        rangeIRGenerator.generateConstructor(r) { funName =>
          TCN.Field(TCN.Var(funName), "apply")
        }
      }
  }

  /**
  * Generates a code pattern for a term.
  * 
  * @return the pattern and a mapping from collection variables (e.g. `es`) to their element variables (e.g. `e`), if any 
  */
  def generatePattern(term: Term): (TargetCodePattern, Map[String, String]) = term match {
    case Term.Constant(value: Int) => (TCP.Integer(value), Map())
    case Term.Constant(value: String) => (TCP.Text(value), Map())
    case Term.Constant(_) => ???
    case Term.Variable(name) => (TCP.Var(name), Map())
    case Term.Function(name, args*) => 
      val (argPats, colVars) = args.map(generatePattern).unzip
      (TCP.ADTConstructor(TCTypeRef(name), argPats*), colVars.fold(Map())(_ ++ _))
    case Term.Type(typ) => (typeIRGenerator.generatePattern(typ), Map())
    case Term.Label(label) => (labelIRGenerator.generatePattern(label), Map())
    case r: Term.Range =>
      r.toConcrete(Term.Function(_, _*))
        .map(generatePattern)
        .getOrElse(rangeIRGenerator.generateUnlimitedPattern(r, generatePattern))
  }
