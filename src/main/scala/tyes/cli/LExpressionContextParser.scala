package tyes.cli

import tyes.model.*

import Parsers.*

/** 
  Pretty much the same as example.LExpressionParser but allowing for each sub-expression
  to be a meta-variable, so that it can be used by TyesParser.
 
  If the source parser is assumed to be in Scala, this could perfectly be generated automatically
*/
class LExpressionContextParser(
  metaTermVarParser: Parser[Term],
  newIdentifierTerm: String => Term
):

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r

  def variable = ident ^^ { varName => Term.Function("LVariable", newIdentifierTerm(varName)) } 

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => Term.Function("LNumber", Term.Constant(numStr.toInt)) }
  
  def leaf = ("(" ~> expression <~ ")") | number | (metaTermVarParser ||| variable)
  
  def operator = leaf ~ ("+" ~> leaf).* ^^ { 
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => Term.Function("LPlus", left, right) }
  }
  
  def let = ("let" ~> ident) ~ ("=" ~> operator) ~ ("in" ~> expression) ^^ {
    case varName ~ varExp ~ inExp => Term.Function("LLet", newIdentifierTerm(varName), varExp, inExp)
  }
  
  def expression: Parser[Term] = let | operator

object LExpressionLanguageBindings extends TyesTermLanguageBindings:

  def buildTermLanguageParser(
    metaTermVariableParser: Parser[Term],
    newIdentifierTerm: String => Term
  ) = new LExpressionContextParser(metaTermVariableParser, newIdentifierTerm).expression
