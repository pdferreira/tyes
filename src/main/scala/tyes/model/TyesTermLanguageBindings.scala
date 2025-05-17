package tyes.model

import Parsers.*
import tyes.model.indexes.*
import tyes.model.ranges.*

trait TyesTermLanguageBindings:

  def metaTermVariableParser: Parser[Term]
  
  def identTermParser(ident: String): Parser[Term]

  def typeParser: Parser[Type]

  def rep1op(elem: Parser[Term], op: Parser[String], funName: String): Parser[Term] =
    Parsers.rep1sep(elem, op) ~ (op ~> "..." ~>! op ~>! Parsers.rep1sep(elem, op)).? flatMap {
      case List(exp, rs*) ~ None =>
        success(rs.foldLeft(exp) { (left, right) => Term.Function(funName, left, right) })
      case (ls @ List(_, _*)) ~ Some(r +: rs) =>
        val start = ls.last
        val end = r
        val seed =
          if ls.size == 1 then
            None
          else 
            Some(ls.tail.dropRight(1).foldLeft(ls.head) { (left, right) => Term.Function(funName, left, right) })

        extractTermRange(funName, start, end, seed, minOccurs = 2, Term.Range.apply)
          .map(r => rs.foldLeft(r: Term) { (left, right) => Term.Function(funName, left, right) })
          .fold(msgs => err(msgs.head), success)
      case Nil ~ _ => err("impossible")
      case _ ~ Some(_) => err("impossible")
    }