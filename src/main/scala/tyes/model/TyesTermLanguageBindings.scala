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
        extractTermRange(funName, start, end, None)
          .map(r => {
            // since this is an operator, the range should match at least two elements
            // otherwise it should defer to other rules
            val secondElem = r.template.replaceIndex(r.cursor, (r.minIndex + 1).toString)
            val seed = Some((ls.tail :+ secondElem).foldLeft(ls.head) { (left, right) => Term.Function(funName, left, right) })
            val range = r.copy(minIndex = r.minIndex + 2, seed = seed)

            rs.foldLeft(range: Term) { (left, right) => Term.Function(funName, left, right) }
          })
          .fold(msgs => err(msgs.head), success)
      case Nil ~ _ => err("impossible")
      case _ ~ Some(_) => err("impossible")
    }