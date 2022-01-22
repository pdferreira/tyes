package tyes.model

import Parsers.*

trait TyesParser(contextParserBuilder: TermContextParserBuilder):
  def ident = raw"[a-zA-Z][a-zA-Z\d_']*".r
  def typesystem = "typesystem" ~> ident.? ~ rule.+ ^^ { case nameOpt ~ rules => TypeSystemDecl(nameOpt, rules) }
  def rule = ("rule" ~> ident.?) ~ ("infers" ~> assertion) ~ ("if" ~> rep1sep(assertion, "and")).? ^^ { 
    case nameOpt ~ concl ~ premisesOpt  => RuleDecl(nameOpt, premisesOpt.getOrElse(Seq()), concl) 
  }
  def assertion = term ~ (":" ~> tpe) ^^ { case term ~ tpe => HasType(term, tpe) }
  def metaVariable = ident ^^ { varName => Term.Variable(varName) }
  def term = contextParserBuilder(metaVariable)
  def tpe = ident ^^ { case name => Type.Named(name) }
  def parse(input: String) = Parsers.parse(phrase(typesystem), input)
