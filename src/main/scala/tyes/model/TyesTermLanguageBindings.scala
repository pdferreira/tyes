package tyes.model

import Parsers.*

trait TyesTermLanguageBindings:

  def metaTermVariableParser: Parser[Term]
  
  def identTermParser(ident: String): Parser[Term]

  def typeParser: Parser[Type]
