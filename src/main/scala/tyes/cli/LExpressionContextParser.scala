package tyes.cli

import tyes.model.*
import tyes.model.indexes.*

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

  def list = "[" ~> bindings.rep0opR(expression, ",", "LList") { 
    ("|" ~> metaVariable).? ^^ { tail => tail.getOrElse(Term.Function("LNil")) } 
  } <~ "]"
  
  def leaf = 
    ("(" ~> expression <~ ")") 
    | number 
    | variable
    | list

  def app = bindings.rep1opL(leaf, "", "LApp")

  def operator = bindings.rep1opL(app, "+", "LPlus")

  def tpe = bindings.typeParser
  
  def let: Parser[Term] = bindings.repXopR(
    ("let" ~> ident) ~ (":" ~> tpe).? ~ ("=" ~> (expression - let)) ^^ {
      case varTerm ~ varTypeOpt ~ varExp => Seq(
        varTerm,
        Term.Type(varTypeOpt.getOrElse(Constants.Types.any)),
        varExp,
        )
    },
    "in",
    "LLet",
    atLeastOne = true
   ) { "in" ~> expression }

  def fun: Parser[Term] = bindings.repXopR(
    ("fun" ~> ident) ~ (":" ~> tpe).? ^^ {
      case argTerm ~ argTypeOpt => Seq(
        argTerm,
        Term.Type(argTypeOpt.getOrElse(Constants.Types.any))
      ) 
    },
    "=>",
    "LFun",
    atLeastOne = true,
  ) { "=>" ~> expression }
  
  def expression: Parser[Term] = let | fun | operator

object LExpressionContextParser extends (TyesTermLanguageBindings => Parser[Term]):

  def apply(bindings: TyesTermLanguageBindings) = new LExpressionContextParser(bindings).expression
