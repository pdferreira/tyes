package tyes.model

import Parsers.*

trait TyesParser(buildTermLanguageParser: TyesTermLanguageBindings => Parser[Term]):

  val keywords = Set("typesystem", "rule", "infers", "if", "and", "under")
  
  // All variables and declaration names: int, Lambda, e1
  def genericIdent = raw"[a-zA-Z][a-zA-Z\d_']*".r.filter(id => !keywords.contains(id))
  
  // Generic meta variables: e, e1, e'
  def metaIdent = raw"[a-z](\d+|'+)?".r

  // Meta variables conventioned to match (synctatic) variables: x, y', z2, z_i
  def metaVarIdent = raw"[f-hx-z](\d+|'+|(_(\d+|[a-z])))?".r
  
  // Names: 
  def declIdent = genericIdent.filter(id => !metaIdent.matches(id))

  def typesystem = "typesystem" ~> declIdent.? ~ rule.+ ^^ { case nameOpt ~ rules => TypeSystemDecl(nameOpt, rules) }
  
  def rule = ("rule" ~> declIdent.?) ~ ("infers" ~> judgement) ~ ("if" ~> rep1sep(premise, "and")).? ^^ { 
    case nameOpt ~ concl ~ premisesOpt  => RuleDecl(nameOpt, premisesOpt.getOrElse(Seq()), concl) 
  }
  
  def assertion = term ~ (":" ~> tpe) ^^ { case term ~ tpe => HasType(term, tpe) }

  def premise: Parser[Premise] = judgement ~ ("and" ~ "..." ~ "and" ~> judgement).? ^^ {
    case judg ~ None => judg
    case fromJudg ~ Some(toJudg) => JudgementRange(fromJudg, toJudg)
  }
  
  def judgement = assertion ~ ("under" ~> environment).? ^^ { case assert ~ envOpt => 
    val env = envOpt.getOrElse(Environment(Seq()))
    Judgement(env, assert) 
  }
  
  def environment = rep1sep(environmentPart, ",") ^^ { case parts => Environment(parts) }

  def environmentPart = 
    genericIdent ^^ { case name => EnvironmentPart.Variable(name) }
    ||| rep1sep(binding, ",") ^^ { case bs => EnvironmentPart.Bindings(bs) }

  def binding = genericIdent ~ (":" ~> tpe) ^^ { case name ~ tpe => 
    if metaVarIdent.matches(name)
    then Binding.BindVariable(name, tpe)
    else Binding.BindName(name, tpe) 
  }
  
  def metaVarIndex =
    ("0" | raw"[1-9]\d*".r) ^^ { numStr => Term.Constant(numStr.toInt) }
    | raw"[a-z]".r ^^ { varName => Term.Variable(varName, None) }

  def metaTermVariable = metaIdent.filter(id => !metaVarIdent.matches(id)) ~ ("_" ~> metaVarIndex).? ^^ {
    case varName ~ idxTermOpt => Term.Variable(varName, index = idxTermOpt)
  }

  def newIdentifierTerm(ident: String): Term =
    if metaVarIdent.matches(ident)
    then Term.Variable(ident, index = None)
    else Term.Constant(ident)
  
  def term: Parser[Term] = buildTermLanguageParser(new TyesTermLanguageBindings {
    def metaTermVariableParser = metaTermVariable
    def identTermParser(ident: String) = Parsers.success(newIdentifierTerm(ident))
    def typeParser = tpe
  })
  
  def leafType = 
    genericIdent ^^ { case name => 
      if metaIdent.matches(name) 
      then Type.Variable(name) 
      else Type.Named(name) 
    }
    | ("(" ~> tpe <~ ")")

  def tpe: Parser[Type] = leafType ~ (Constants.Types.Function.operator ~> tpe).? ^^ { 
    case argTpe ~ None => argTpe
    case argTpe ~ Some(retTpe) => Constants.Types.Function(argTpe, retTpe)
  }

  def parse(input: String) = Parsers.parse(phrase(typesystem), input)
