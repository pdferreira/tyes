package tyes.cli

import tyes.model.*

import Parsers.*

/** 
  Pretty much the same as example.LExpressionParser but allowing for each sub-expression
  to be a meta-variable, so that it can be used by TyesParser.
 
  If the source parser is assumed to be in Scala, this could perfectly be generated automatically
*/
class LExpressionContextParser(metaVariableParser: Parser[Term]):

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r

  def variable = ident ^^ { varName => Term.Function("LVariable", Term.Constant(varName)) } 

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => Term.Function("LNumber", Term.Constant(numStr.toInt)) }
  
  def leaf = number | metaVariableParser ||| variable
  
  def operator = leaf ~ ("+" ~> leaf).* ^^ { 
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => Term.Function("LPlus", left, right) }
  }
  
  def expression: Parser[Term] = operator

object LExpressionContextParser extends TermContextParserBuilder:

  def apply(metaVariableParser: Parser[Term]) = new LExpressionContextParser(metaVariableParser).expression
