package tyes.model

import Parsers.*

trait TyesTermLanguageBindings:
  
  def buildTermLanguageParser(
    metaTermVariableParser: Parser[Term],
    newIdentifierTerm: String => Term
  ): Parser[Term]
