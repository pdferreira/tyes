package tyes.model

import Parsers.*

trait TyesParser(termLanguageBindings: TyesTermLanguageBindings):

  val keywords = Set("typesystem", "rule", "infers", "if", "and", "under")
  
  // All variables and declaration names: int, Lambda, e1
  def genericIdent = raw"[a-zA-Z][a-zA-Z\d_']*".r.filter(id => !keywords.contains(id))
  
  // Generic meta variables: e, e1, e'
  def metaIdent = raw"[a-z](\d+|'+)?".r

  // Meta variables conventioned to match (synctatic) variables: x, y', z2
  def metaVarIdent = raw"[x-z](\d+|'+)?".r
  
  // Names: 
  def declIdent = genericIdent.filter(id => !metaIdent.matches(id))

  def typesystem = "typesystem" ~> declIdent.? ~ rule.+ ^^ { case nameOpt ~ rules => TypeSystemDecl(nameOpt, rules) }
  
  def rule = ("rule" ~> declIdent.?) ~ ("infers" ~> judgement) ~ ("if" ~> rep1sep(judgement, "and")).? ^^ { 
    case nameOpt ~ concl ~ premisesOpt  => RuleDecl(nameOpt, premisesOpt.getOrElse(Seq()), concl) 
  }
  
  def assertion = term ~ (":" ~> tpe) ^^ { case term ~ tpe => HasType(term, tpe) }
  
  def judgement = assertion ~ ("under" ~> environment).? ^^ { case assert ~ env => Judgement(env, assert) }
  
  def environment = genericIdent ~ (":" ~> tpe) ^^ { case name ~ tpe => 
    if metaVarIdent.matches(name)
    then Environment.BindVariable(name, tpe)
    else Environment.BindName(name, tpe) 
  }
  
  def metaTermVariable = metaIdent.filter(id => !metaVarIdent.matches(id)) ^^ { varName => Term.Variable(varName) }

  def newIdentifierTerm(ident: String): Term =
    if metaVarIdent.matches(ident)
    then Term.Variable(ident)
    else Term.Constant(ident)
  
  def term = termLanguageBindings.buildTermLanguageParser(metaTermVariable, newIdentifierTerm)
  
  def tpe = genericIdent ^^ { case name => 
    if metaIdent.matches(name) 
    then Type.Variable(name) 
    else Type.Named(name) 
  }

  def parse(input: String) = Parsers.parse(phrase(typesystem), input)
