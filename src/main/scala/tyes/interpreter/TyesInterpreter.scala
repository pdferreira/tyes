package tyes.interpreter

import tyes.model.*

object TyesInterpreter:
  
  def typecheck[E](tsDecl: TypeSystemDecl, ruleDecl: RuleDecl, term: Term): Option[Type] = ruleDecl.conclusion match {
    case HasType(metaTerm, typ) => metaTerm.matches(term).flatMap { subst => 
      if ruleDecl.premises.forall { 
        case HasType(premTerm, premTyp) => typecheck(tsDecl, premTerm.substitute(subst)) == Some(premTyp)
      }
      then
        Some(typ)
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
