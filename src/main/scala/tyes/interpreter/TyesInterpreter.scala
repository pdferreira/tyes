package tyes.interpreter

import tyes.model.*
import tyes.model.TyesLanguageExtensions.*

object TyesInterpreter:

  private val DefaultEnv = 
    val defaultVarPart = EnvironmentPart.Variable("$env")
    Environment(Seq(defaultVarPart))
  
  private def getSelfOrDefaultIfEmpty(env: Environment, default: Environment): Environment =
    if env.parts.isEmpty 
    then default
    else env

  def typecheck(tsDecl: TypeSystemDecl, ruleDecl: RuleDecl, term: Term, termEnv: Map[String, Type]): Option[Type] = ruleDecl.conclusion match {
    case Judgement(metaEnv, HasType(metaTerm, typ)) =>
      metaTerm.matches(term).flatMap { (termSubst) =>
        // replace the vars we already unified in the term
        // TODO: review assumption that only string constants that are directly unified are variables
        val termVarSubst = termSubst.collect { case (k, Term.Constant(varName: String)) => k -> varName }
        val typeVarSubst = termSubst.collect { case (k, Term.Type(t)) => k -> t }
        val refinedMetaEnv = getSelfOrDefaultIfEmpty(metaEnv, DefaultEnv)
          .substitute(EnvironmentMatch(termVarSubst, typeVarSubst))

        // match the conclusion env to the rule to see if it's applicable at all
        refinedMetaEnv.matches(termEnv).flatMap { case m @ EnvironmentMatch(envTermVarSubst, envTypeVarSubst, envVarSubst) =>
          val envTermSubst = envTermVarSubst.mapValues(Term.Constant(_)).toMap
          val refinedMetaTerm = metaTerm.substitute(envTermSubst)

          // build variable substitutions considering info from term and environment
          val allVarSubst = envTermVarSubst ++ termVarSubst
          val allTypeVarSubst = envTypeVarSubst ++ typeVarSubst
          val allTermSubst = envTermSubst ++ termSubst

          // check all premises hold, while unifying possible type variables
          val premTypeCheckResult = ruleDecl.premises.foldLeft(Option(allTypeVarSubst)) { 
            // if one of the premises failed, propagate failure
            case (None, _) => None 
            // otherwise, check the next premise
            case (Some(typeVarEnv), Judgement(premMetaEnv, HasType(premTerm, premTyp))) => 
              // replace the term and type variables we already know in the premise env and then produce an
              // actual term env out of it
              val premEnvMatch = EnvironmentMatch(allVarSubst, typeVarEnv, envVarSubst)
              val premEnvOpt = getSelfOrDefaultIfEmpty(premMetaEnv, refinedMetaEnv).substitute(premEnvMatch).toConcrete

              val refinedPremTerm = premTerm.substitute(allTermSubst)
              val resTyp = premEnvOpt.flatMap(premEnv => typecheck(tsDecl, refinedPremTerm, premEnv))
              for
                resT <- resTyp
                premTypeSubst <- premTyp.substitute(typeVarEnv).matches(resT)
              yield
                // if the premise expected type matched the obtained, update the type env with any new unifications
                typeVarEnv ++ premTypeSubst
          }

          premTypeCheckResult.map(typ.substitute(_))
        }
      }
  }

  def typecheck(tsDecl: TypeSystemDecl, term: Term, env: Map[String, Type]): Option[Type] =
    tsDecl
      .rules
      .collectFirst(Function.unlift(r => typecheck(tsDecl, r, term, env)))
  
  def typecheck[E](tsDecl: TypeSystemDecl, exp: E)(using Conversion[E, Term]): Option[Type] =
    typecheck(tsDecl, exp.convert, Map())
