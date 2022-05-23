package tyes.interpreter

import tyes.model.*
import TyesLanguageExtensions.*

object TyesInterpreter:

  def typecheck(tsDecl: TypeSystemDecl, ruleDecl: RuleDecl, term: Term, termEnv: Map[String, Type.Named]): Option[Type] = ruleDecl.conclusion match {
    case Judgement(metaEnvOpt, HasType(metaTerm, typ)) =>
      // match the conclusion env to the rule to see if it's applicable at all
      val typSubstOpt = metaEnvOpt match {
        case None => Some(Map())
        case Some(metaEnv) => metaEnv.matches(termEnv)
      }
      
      typSubstOpt.zip(metaTerm.matches(term)).flatMap { (typeSubst, subst) =>
        // check all premises hold, while unifying possible type variables
        val (premisesHold, finalTypeVarEnv) = ruleDecl.premises.foldLeft((true, typeSubst)) { 
          // if one of the premises failed, propagate failure
          case (acc @ (false, typeVarEnv), _) => acc 
          // otherwise, check the next premise
          case ((true, typeVarEnv), Judgement(premMetaEnvOpt, HasType(premTerm, premTyp))) => 
            // replace the type variables we already know in the premise env and then produce an
            // actual term env out of it
            val premEnvOpt = premMetaEnvOpt.map(_.substitute(typeVarEnv).toConcrete).getOrElse(Some(Map()))
            val resTyp = premEnvOpt.flatMap(premEnv => typecheck(tsDecl, premTerm.substitute(subst), premEnv))
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

  def typecheck(tsDecl: TypeSystemDecl, term: Term, env: Map[String, Type.Named]): Option[Type] =
    tsDecl
      .rules
      .collectFirst(Function.unlift(r => typecheck(tsDecl, r, term, env)))
  
  def typecheck[E](tsDecl: TypeSystemDecl, exp: E)(using Conversion[E, Term]): Option[Type] =
    typecheck(tsDecl, exp.convert, Map())
