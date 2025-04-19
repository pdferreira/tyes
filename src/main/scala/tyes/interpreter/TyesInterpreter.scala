package tyes.interpreter

import scala.util.matching.Regex
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
            case (Some(typeVarEnv), judg: Judgement) =>
              val premEnvMatch = EnvironmentMatch(allVarSubst, typeVarEnv, envVarSubst)
              typecheck(tsDecl, judg, premEnvMatch, refinedMetaEnv, allTermSubst)
            case (Some(typeVarEnv), JudgementRange(from, to)) =>
              val Judgement(fromEnv, HasType(Term.Variable(fromVar), fromTyp)) = from
              val Judgement(toEnv, HasType(Term.Variable(toVar), toTyp)) = to
              assert(fromEnv == toEnv, "Both from and to premise must share the environment")
              assert(fromTyp == toTyp, "Both from and to premise must share the judgement type")

              val Array(fromIdent, fromIdxStr) = fromVar.split("_")
              val Array(toIdent, toIdxStr) = toVar.split("_")
              assert(fromIdent == toIdent, "Both from and to premise must share the judgement var minus index") // todo: generalize to matched terms?
              
              val fromIdx = fromIdxStr.toInt
              val toIdx = toIdxStr.toIntOption.getOrElse(Int.MaxValue)
              // Assumption all the variable indexes are bound in the conclusion
              val premsToConsider = for
                (termVar, term) <- allTermSubst
                if termVar.matches(Regex.quote(fromIdent) + "_" + ".+")
                currIdx = termVar.split("_")(1).toInt
                if currIdx >= fromIdx && currIdx <= toIdx
              yield
                Judgement(fromEnv, HasType(term, fromTyp))

              premsToConsider.foldLeft(Option(typeVarEnv)) {
                case (None, _) => None
                case (Some(pTypeVarEnv), judg) =>
                  val premEnvMatch = EnvironmentMatch(allVarSubst, pTypeVarEnv, envVarSubst)
                  typecheck(tsDecl, judg, premEnvMatch, refinedMetaEnv, allTermSubst)
              }
          }

          premTypeCheckResult.map(typ.substitute(_))
        }
      }
  }

  def typecheck(
    tsDecl: TypeSystemDecl,
    judgement: Judgement,
    premEnvMatch: EnvironmentMatch,
    refinedMetaEnv: Environment,
    allTermSubst: Map[String, Term]
  ): Option[Map[String, Type]] =
    val Judgement(premMetaEnv, HasType(premTerm, premTyp)) = judgement: @unchecked
    
    // replace the term and type variables we already know in the premise env and then produce an
    // actual term env out of it
    val premEnvOpt = getSelfOrDefaultIfEmpty(premMetaEnv, refinedMetaEnv).substitute(premEnvMatch).toConcrete

    val refinedPremTerm = premTerm.substitute(allTermSubst)
    val resTyp = premEnvOpt.flatMap(premEnv => typecheck(tsDecl, refinedPremTerm, premEnv))
    for
      resT <- resTyp
      premTypeSubst <- premTyp.substitute(premEnvMatch.typeVarSubst).matches(resT)
    yield
      // if the premise expected type matched the obtained, update the type env with any new unifications
      premEnvMatch.typeVarSubst ++ premTypeSubst

  def typecheck(tsDecl: TypeSystemDecl, term: Term, env: Map[String, Type]): Option[Type] =
    tsDecl
      .rules
      .collectFirst(Function.unlift(r => typecheck(tsDecl, r, term, env)))
  
  def typecheck[E](tsDecl: TypeSystemDecl, exp: E)(using Conversion[E, Term]): Option[Type] =
    typecheck(tsDecl, exp.convert, Map())
