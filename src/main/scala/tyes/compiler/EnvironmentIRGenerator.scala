package tyes.compiler

import tyes.compiler.ir.IRError
import tyes.compiler.ir.IRInstr
import tyes.compiler.ir.IRNode
import tyes.compiler.target.TargetCodeNode
import tyes.compiler.target.TargetCodeTypeRef
import tyes.model.*
import tyes.model.TyesLanguageExtensions.*
import utils.StringExtensions.*
import utils.collections.*

private type TCTypeRef = TargetCodeTypeRef
private type TCN = TargetCodeNode
private val TCN = TargetCodeNode

class EnvironmentIRGenerator(
  private val typeIRGenerator: TypeIRGenerator,
  private val commonEnvName: String
):
  
  private val envVar: TCN.Var = genVarCode(commonEnvName)
  private val envClassTypeRef = TCTypeRef("Environment")

  private def genVarCode(envName: String): TCN.Var = TCN.Var(envName.decapitalize)

  def generateParameter(): (String, TCTypeRef) = 
    envVar.name -> envClassTypeRef.copy(params = Seq(typeIRGenerator.typeEnumTypeRef))
  
  def generate(env: Environment, codeEnv: TargetCodeEnv): TCN =
    assert(!env.parts.isEmpty)
    env.parts
      .map(generate(_, codeEnv))
      .foldLeft1((c1, c2) => c2 match {
        // Semantically equivalent special case just to reduce verbosity of 
        // `env + Environment(x1 -> t1, x2 -> t2)` to `env + (x1 -> t1) + (x2 -> t2)`
        case TCN.ADTConstructorCall(`envClassTypeRef`, exps*) =>
          exps.foldLeft(c1)((ce1, ce2) => TCN.InfixApply(ce1, "+", ce2))
        case _ => 
          TCN.InfixApply(c1, "+", c2)
      })

  private def generate(binding: Binding, codeEnv: TargetCodeEnv): TCN =
    val (varNameExpr, typ) = generateBinding(binding)
    TCN.Entry(varNameExpr, typeIRGenerator.generate(typ, codeEnv))

  private def generate(env: EnvironmentPart, codeEnv: TargetCodeEnv): TCN = env match {
    case EnvironmentPart.Bindings(bindings) =>
      val entryExprs = bindings.map(generate(_, codeEnv))
      TCN.ADTConstructorCall(envClassTypeRef, entryExprs*)

    case EnvironmentPart.Variable(name) =>
      assert(
        genVarCode(name) == envVar, 
        s"Compilation of rules with distinct environment variables is not supported. Expected `${envVar.name}`, got `$name`")
      
      envVar
  }

  def generateConditions(env: Environment, codeEnv: TargetCodeEnv): Iterable[IRInstr] =
    genSizeConds(env) ++ genContentConds(env, codeEnv)

  private def genContentConds(env: Environment, codeEnv: TargetCodeEnv) =
    for 
      // Iterate all bindings
      case EnvironmentPart.Bindings(bs) <- env.parts
      (varNameExpr, typ) <- bs.map(generateBinding)

      // Generate the env.get, type expectation and target pattern code
      varGetCode = TCN.Apply(TCN.Field(envVar, "get"), varNameExpr)
      checkedVarGetCode = typeIRGenerator.generateExpectationCheck(typ, codeEnv, varGetCode)
      
      // Yield all the conds necessary to decl, destructure and cross-check the type with other occurrences
      c <- typeIRGenerator.generateDestructureDecl(typ, codeEnv, IRNode.Result(checkedVarGetCode, canFail = true))
    yield
      c

  private def genSizeConds(env: Environment) =
    assert(!env.parts.isEmpty)

    val sizeOpt = 
      if !env.envVariables.isEmpty
      then None
      else Some(env.parts.collect({ case EnvironmentPart.Bindings(bs) => bs.length }).sum)

    for size <- sizeOpt yield 
      IRInstr.Check(
        exp = IRNode.Result(RuntimeAPIGenerator.genCheckEnvSize(envVar, size), canFail = true),
        resPat = TCP.Any
      )

  private def generateBinding(binding: Binding): (TargetCodeNode, Type) = binding match {
    case Binding.BindName(name, typ) => (TCN.Text(name), typ)
    case Binding.BindVariable(name, typ) => (TCN.Var(name), typ)
  }