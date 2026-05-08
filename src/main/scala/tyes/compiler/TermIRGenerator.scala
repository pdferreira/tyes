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
  * @return the pattern 
  */
  def generatePattern(
    term: Term,
    codeEnv: TargetCodeEnv,
    colToElemVar: Map[String, String] = Map(),
  ): TargetCodePattern = term match {
    case Term.Constant(value: Int) => TCP.Integer(value)
    case Term.Constant(value: String) => TCP.Text(value)
    case Term.Constant(_) => ???
    case v @ Term.Variable(name) =>
      val (_, varCode) = codeEnv.requestIdentifier(v, elementVar = colToElemVar.get(name))
      TCP.Var(varCode.name)
    case Term.Function(name, args*) => 
      val argPats = args.map(a => generatePattern(a, codeEnv, colToElemVar))
      TCP.ADTConstructor(TCTypeRef(name), argPats*)
    case Term.Type(typ) => typeIRGenerator.generatePattern(typ, codeEnv, colToElemVar)
    case Term.Label(label) => labelIRGenerator.generatePattern(label, codeEnv, colToElemVar)
    case r: Term.Range =>
      r.toConcrete(Term.Function(_, _*))
        .map(f => generatePattern(f, codeEnv, colToElemVar))
        .getOrElse(rangeIRGenerator.generateUnlimitedPattern(r, generatePattern, codeEnv))
  }
