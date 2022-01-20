package tyes.interpreter

import tyes.model.*

object TyesInterpreter:
  
  def typecheck[E](tsDecl: TypeSystemDecl, exp: E)(using Conversion[E, Term]): Option[Type] =
    tsDecl
      .rules
      .map(r => r.conclusion)
      .collectFirst({
        case HasType(term, typ) if term.matches(exp.convert).isDefined => typ
      })
