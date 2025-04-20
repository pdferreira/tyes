package tyes.cli

import tyes.model.*

import Parsers.*

/** 
  Pretty much the same as example.LExpressionParser but allowing for each sub-expression
  to be a meta-variable, so that it can be used by TyesParser.
 
  If the source parser is assumed to be in Scala, this could perfectly be generated automatically
*/
class LExpressionContextParser(bindings: TyesTermLanguageBindings):

  val keywords = Set("let", "in", "fun")

  def ident = raw"[a-zA-Z][a-zA-Z\d_]*".r
    .filter(id => !keywords.contains(id))
    .into(bindings.identTermParser)

  def metaVariable = bindings.metaTermVariableParser - oneOf(keywords)

  def variable = (
    metaVariable
    ||| ident ^^ { varTerm => Term.Function("LVariable", varTerm) }
  )

  def number = ("0" | raw"[1-9]\d*".r) ^^ { numStr => Term.Function("LNumber", Term.Constant(numStr.toInt)) }

  def list = "[" ~> repsep(expression, ",") ~ ("|" ~> metaVariable).? <~ "]" ^^ {
    case elems ~ tail =>
      val tailTerm = tail.getOrElse(Term.Function("LNil")) 
      elems.foldRight(tailTerm) { (e, l) => Term.Function("LList", e, l) }
  }
  
  def leaf = 
    ("(" ~> expression <~ ")") 
    | number 
    | variable
    | list

  def app = leaf ~ leaf.* ^^ {
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => Term.Function("LApp", left, right) }
  }

  def operator = app ~ ("+" ~> app).* ^^ { 
    case exp ~ rs => rs.foldLeft(exp) { (left, right) => Term.Function("LPlus", left, right) }
  }

  def tpe = bindings.typeParser
  
  def let: Parser[Term] = ("let" ~> ident) ~ (":" ~> tpe).? ~ ("=" ~> (expression - let)) ~ ("in" ~> expression) ^^ {
    case varTerm ~ varTypeOpt ~ varExp ~ inExp => 
      Term.Function(
        "LLet",
        varTerm,
        Term.Type(varTypeOpt.getOrElse(Constants.Types.any)),
        varExp,
        inExp)
  }

  def fun: Parser[Term] = ("fun" ~> ident) ~ (":" ~> tpe).? ~ ("=>" ~> expression) ^^ {
    case argTerm ~ argTypeOpt ~ bodyExp =>
      Term.Function(
        "LFun",
        argTerm,
        Term.Type(argTypeOpt.getOrElse(Constants.Types.any)), 
        bodyExp)
  }
  
  def expression: Parser[Term] = let | fun | operator

object LExpressionContextParser extends (TyesTermLanguageBindings => Parser[Term]):

  def apply(bindings: TyesTermLanguageBindings) = new LExpressionContextParser(bindings).expression
