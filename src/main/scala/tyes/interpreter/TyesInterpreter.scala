package tyes.interpreter

import tyes.model.*

object TyesInterpreter:
  
  def typecheck[E](tsDecl: TypeSystemDecl, ruleDecl: RuleDecl, term: Term): Option[Type] = ruleDecl.conclusion match {
    case HasType(metaTerm, typ) => metaTerm.matches(term).flatMap { subst => 
      // check all premises hold, while unifying possible type variables
      val (premisesHold, typeVarEnv) = ruleDecl.premises.foldLeft((true, Map[String, Type.Named]())) { 
        // if one of the premises failed, propagate failure
        case (acc @ (false, typeVarEnv), _) => acc 
        // otherwise, check the next premise
        case ((true, typeVarEnv), HasType(premTerm, premTyp)) => 
          val resTyp = typecheck(tsDecl, premTerm.substitute(subst))
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
        Some(typ match {
          case Type.Named(_) => typ
          case Type.Variable(name) => typeVarEnv(name)
        })
      else
        None
    }
  }

  def typecheck(tsDecl: TypeSystemDecl, term: Term): Option[Type] =
    tsDecl
      .rules
      .collectFirst(Function.unlift(r => typecheck(tsDecl, r, term)))
  
  def typecheck[E](tsDecl: TypeSystemDecl, exp: E)(using Conversion[E, Term]): Option[Type] =
    typecheck(tsDecl, exp.convert)
