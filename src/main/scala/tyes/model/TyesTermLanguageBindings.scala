package tyes.model

import Parsers.*

trait TyesTermLanguageBindings:
  
  def buildTermLanguageParser(metaVariableParser: Parser[Term]): Parser[Term]

  def buildVariableTerm(variableNameTerm: Term): Term