package tyes.cli

import tyes.model.*

import Parsers.*

/** 
  Pretty much the same as example.LExpressionParser but allowing for each sub-expression
  to be a meta-variable, so that it can be used by TyesParser.
 
  If the source parser is assumed to be in Scala, this could perfectly be generated automatically
*/
class LExpressionContextParser(bindings: TyesTermLanguageBindings):

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r.into(bindings.identTermParser)

  def variable = ident ^^ { varTerm => Term.Function("LVariable", varTerm) } 

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => Term.Function("LNumber", Term.Constant(numStr.toInt)) }
  
  def leaf = ("(" ~> expression <~ ")") | number | (bindings.metaTermVariableParser ||| variable)
  
  def operator = leaf ~ ("+" ~> leaf).* ^^ { 
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => Term.Function("LPlus", left, right) }
  }

  def tpe = bindings.typeParser
  
  def let = ("let" ~> ident) ~ (":" ~> tpe).? ~ ("=" ~> operator) ~ ("in" ~> expression) ^^ {
    case varTerm ~ varTypeOpt ~ varExp ~ inExp => 
      Term.Function(
        "LLet",
        varTerm,
        Term.Type(varTypeOpt.getOrElse(Constants.Types.any)),
        varExp,
        inExp)
  }
  
  def expression: Parser[Term] = let | operator

object LExpressionContextParser extends (TyesTermLanguageBindings => Parser[Term]):

  def apply(bindings: TyesTermLanguageBindings) = new LExpressionContextParser(bindings).expression
