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

  def typecheck(tsDecl: TypeSystemDecl, ruleDecl: RuleDecl, term: Term, termEnv: Map[String, Type.Named]): Option[Type] = ruleDecl.conclusion match {
    case Judgement(metaEnv, HasType(metaTerm, typ)) =>
      metaTerm.matches(term).flatMap { (termSubst) =>
        // replace the vars we already unified in the term
        // TODO: review assumption that only string constants that are directly unified are variables
        val termVarSubst = termSubst.collect { case (k, Term.Constant(varName: String)) => k -> varName }
        val refinedMetaEnv = getSelfOrDefaultIfEmpty(metaEnv, DefaultEnv).substitute(EnvironmentMatch(termVarSubst))

        // match the conclusion env to the rule to see if it's applicable at all
        refinedMetaEnv.matches(termEnv).flatMap { case m @ EnvironmentMatch(envTermVarSubst, envTypeVarSubst, envVarSubst) =>
          val envTermSubst = envTermVarSubst.mapValues(Term.Constant(_)).toMap
          val refinedMetaTerm = metaTerm.substitute(envTermSubst)

          // build variable substitutions considering info from term and environment
          val allVarSubst = envTermVarSubst ++ termVarSubst
          val allTermSubst = envTermSubst ++ termSubst

          // check all premises hold, while unifying possible type variables
          val (premisesHold, finalTypeVarEnv) = ruleDecl.premises.foldLeft((true, envTypeVarSubst)) { 
            // if one of the premises failed, propagate failure
            case (acc @ (false, typeVarEnv), _) => acc 
            // otherwise, check the next premise
            case ((true, typeVarEnv), Judgement(premMetaEnv, HasType(premTerm, premTyp))) => 
              // replace the term and type variables we already know in the premise env and then produce an
              // actual term env out of it
              val premEnvMatch = EnvironmentMatch(allVarSubst, typeVarEnv, envVarSubst)
              val premEnvOpt = getSelfOrDefaultIfEmpty(premMetaEnv, refinedMetaEnv).substitute(premEnvMatch).toConcrete

              val refinedPremTerm = premTerm.substitute(allTermSubst)
              val resTyp = premEnvOpt.flatMap(premEnv => typecheck(tsDecl, refinedPremTerm, premEnv))
              premTyp match {
                // if we expect a named type, just compare directly
                case t @ Type.Named(_) => (resTyp == Some(t), typeVarEnv)
                // otherwise, check if we already know what to match the type variable against
                // and if we don't then associate the premise result type
                case Type.Variable(name) =>
                  if typeVarEnv.contains(name) 
                  then (resTyp == Some(typeVarEnv(name)), typeVarEnv)
                  else resTyp match {
                    case Some(t @ Type.Named(_)) => (true, typeVarEnv + (name -> t))
                    case _ => (false, typeVarEnv)
                  }
              }
          }

          if premisesHold then
            Some(typ.substitute(finalTypeVarEnv))
          else
            None
        }
      }
  }

  def typecheck(tsDecl: TypeSystemDecl, term: Term, env: Map[String, Type.Named]): Option[Type] =
    tsDecl
      .rules
      .collectFirst(Function.unlift(r => typecheck(tsDecl, r, term, env)))
  
  def typecheck[E](tsDecl: TypeSystemDecl, exp: E)(using Conversion[E, Term]): Option[Type] =
    typecheck(tsDecl, exp.convert, Map())
