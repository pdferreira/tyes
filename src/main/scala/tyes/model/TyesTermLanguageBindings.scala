package tyes.model

import Parsers.*

trait TyesTermLanguageBindings:

  def metaTermVariableParser: Parser[Term]
  
  def identTermParser(ident: String): Parser[Term]

  def labelParser(ident: String): Parser[Label]

  def typeParser: Parser[Type]
