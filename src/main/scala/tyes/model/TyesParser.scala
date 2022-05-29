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
  
  def environment = declIdent ~ (":" ~> tpe) ^^ { case name ~ tpe => Environment.BindName(name, tpe) }
  
  def metaVariable = metaIdent ^^ { varName => 
    if metaVarIdent.matches(varName) 
    then termLanguageBindings.buildVariableTerm(Term.Variable(varName)) 
    else Term.Variable(varName) 
  }
  
  def term = termLanguageBindings.buildTermLanguageParser(metaVariable)
  
  def tpe = genericIdent ^^ { case name => 
    if metaIdent.matches(name) 
    then Type.Variable(name) 
    else Type.Named(name) 
  }

  def parse(input: String) = Parsers.parse(phrase(typesystem), input)
