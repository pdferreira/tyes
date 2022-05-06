package tyes.cli

import tyes.model.*
import example.*

import Parsers.*

/** 
  Pretty much the same as example.LExpressionParser but allowing for each sub-expression
  to be a meta-variable, so that it can be used by TyesParser.
 
  If the source parser is assumed to be in Scala, this could perfectly be generated automatically
*/
class LExpressionContextParser(metaVariableParser: Parser[Term]):
  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => Term.Function("LNumber", Term.Constant(numStr.toInt)) }
  def leaf = number | metaVariableParser
  def operator = leaf ~ ("+" ~ expression).? ^^ { 
    case exp ~ None => exp
    case left ~ Some(_ ~ right) => Term.Function("LPlus", left, right) 
  }
  def expression: Parser[Term] = operator

object LExpressionContextParser extends TermContextParserBuilder:
  def apply(metaVariableParser: Parser[Term]) = new LExpressionContextParser(metaVariableParser).expression