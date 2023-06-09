package tyes.compiler

import tyes.compiler.ir.IRError
import tyes.compiler.ir.IRInstr
import tyes.compiler.ir.IRNode
import tyes.compiler.ir.TargetCodeNode
import tyes.compiler.ir.TargetCodeTypeRef
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

  private def genVarCode(envName: String): TCN.Var = TCN.Var(envName.decapitalize)

  def generateParameter(): (String, TCTypeRef) = 
    envVar.name -> TCTypeRef("Environment", typeIRGenerator.typeEnumTypeRef)

  
  def generate(env: Environment, codeEnv: TargetCodeEnv): TCN =
    assert(!env.parts.isEmpty)
    env.parts
      .map(generate(_, codeEnv))
      .foldLeft1((c1, c2) => TCN.Apply(TCN.Field(c1, "++"), c2))

  private def generate(binding: Binding, codeEnv: TargetCodeEnv): TCN =
    val (varNameExpr, typ) = generateBinding(binding)
    TCN.Entry(varNameExpr, typeIRGenerator.generate(typ, codeEnv))

  private def generate(env: EnvironmentPart, codeEnv: TargetCodeEnv): TCN = env match {
    case EnvironmentPart.Bindings(bindings) =>
      val entryExprs = bindings.map(generate(_, codeEnv))
      TCN.ADTConstructorCall(TCTypeRef("Environment"), entryExprs*)

    case EnvironmentPart.Variable(name) =>
      assert(
        genVarCode(name) == envVar, 
        s"Compilation of rules with distinct environment variables is not supported. Expected `${envVar.name}`, got `$name`")
      
      envVar
  }

  def generateConditions(env: Environment, codeEnv: TargetCodeEnv): Iterable[IRInstr[TargetCodeNode]] =
    genSizeConds(env) ++ genContentConds(env, codeEnv)

  private def genContentConds(env: Environment, codeEnv: TargetCodeEnv) =
    for 
      case EnvironmentPart.Bindings(bs) <- env.parts
      case (varNameExpr, Type.Variable(typVarName)) <- bs.map(generateBinding)
    yield
      val (realId, realIdCode) = codeEnv.requestIdentifier(typVarName)
      IRInstr.Check(
        exp = IRNode.Result(TCN.Apply(TCN.Field(envVar, "get"), varNameExpr), canFail = true),
        resVar = Some(realId)
      )

  private def genSizeConds(env: Environment) =
    assert(!env.parts.isEmpty)

    val sizeOpt = 
      if !env.envVariables.isEmpty
      then None
      else Some(env.parts.collect({ case EnvironmentPart.Bindings(bs) => bs.length }).sum)

    for size <- sizeOpt yield 
      IRInstr.Check(
        exp = IRNode.Result(RuntimeAPIGenerator.genCheckEnvSize(envVar, size), canFail = true),
        resVar = None
      )

  private def generateBinding(binding: Binding): (TargetCodeNode, Type) = binding match {
    case Binding.BindName(name, typ) => (TCN.Text(name), typ)
    case Binding.BindVariable(name, typ) => (TCN.Var(name), typ)
  }