package tyes.model

import Parsers.*

trait TyesParser(termParser: Parser[Term]):
  def ident = raw"[a-zA-Z][a-zA-Z\d_']*".r
  def typesystem = "typesystem" ~> ident.? ~ rule.+ ^^ { case name ~ rules => TypeSystemDecl(name, rules) }
  def rule = ("rule" ~> ident.?) ~ ("infers" ~> assertion) ^^ { case name ~ concl  => RuleDecl(name, concl) }
  def assertion = term ~ (":" ~> tpe) ^^ { case term ~ tpe => HasType(term, tpe) }
  def term = termParser
  def tpe = ident ^^ { case name => Type.Named(name) }
  def parse(input: String) = Parsers.parse(phrase(typesystem), input)
