package tyes.compiler

import tyes.model.*
import tyes.model.TyesLanguageExtensions.*

/**
 * Desugars `Environment`s with no parts to a single environment variable,
 * preserving semantics.
 */
class TyesEnvDesugarer(commonEnvVar: String):

  def desugar(tsDecl: TypeSystemDecl): TypeSystemDecl =
    tsDecl.copy(rules = tsDecl.rules.map(desugar(_)))

  def desugar(ruleDecl: RuleDecl): RuleDecl =
    val RuleDecl(_, prems, concl) = ruleDecl
    val conclEnvVars = concl.env.envVariables
    if conclEnvVars.size > 1 then
      return ruleDecl

    ruleDecl.copy(
      premises = prems.map(desugar(_, conclEnvVars.headOption)),
      conclusion = desugar(concl, conclEnvVars.headOption)
    )

  def desugar(judg: Judgement, envVarToReplace: Option[String]): Judgement =
    judg.copy(
      env = desugar(judg.env, envVarToReplace)
    )

  def desugar(env: Environment, envVarToReplace: Option[String]): Environment =
    if env.parts.isEmpty then
      Environment(Seq(EnvironmentPart.Variable(commonEnvVar)))
    else
      envVarToReplace match {
        case None => env
        case Some(srcEnvVar) => env.substitute(EnvironmentMatch(envVarSubst = Map(srcEnvVar -> Left(commonEnvVar))))
      }

object TyesEnvDesugarer:

  def inferEnvVarName(tsDecl: TypeSystemDecl): Option[String] =
    // when environments get a more formal declaration in the DSL this might not be needed
    // but for now use a simple "is there a single env name" heuristic
    val allConclEnvVariables = tsDecl.rules.flatMap(_.conclusion.env.envVariables).toSet
    if allConclEnvVariables.size == 1 then
      allConclEnvVariables.headOption
    else
      None
