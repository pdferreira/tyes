package tyes.model

import tyes.model.indexes.*
import tyes.model.ranges.*
import tyes.model.terms.*
import Parsers.*

trait RangeParsers[TTerm <: TermOps[TTerm, TConstant], TConstant]:

  val termBuilder: TermBuilder[TTerm, TConstant]

  import termBuilder.*

  def rep1opL(elem: Parser[TTerm], op: Parser[String], funName: String): Parser[TTerm] =
    Parsers.rep1sep(elem, op) ~ (op ~> "..." ~>! op ~>! Parsers.rep1sep(elem, op)).? flatMap {
      case List(exp, rs*) ~ None =>
        success(rs.foldLeft(exp) { (left, right) => applyFunction(funName, left, right) })
      case (ls @ List(_, _*)) ~ Some(r +: rs) =>
        val start = ls.last
        val end = r
        val seed =
          if ls.size == 1 then
            None
          else 
            Some(ls.tail.init.foldLeft(ls.head) { (left, right) => applyFunction(funName, left, right) })

        extractTermRange(funName, holeArgIdx = 0, Seq(start), Seq(end), seed, minOccurs = 2, applyRange)
          .map(r => rs.foldLeft(r: TTerm) { (left, right) => applyFunction(funName, left, right) })
          .fold(msgs => err(msgs.head), success)
      case Nil ~ _ => err("impossible")
      case _ ~ Some(_) => err("impossible")
    }

  def rep0opR(elem: Parser[TTerm], op: Parser[String], funName: String)(seed: Parser[TTerm]): Parser[TTerm] =
    repXopR(elem.map(Seq(_)), op, funName, atLeastOne = false)(seed)

  def repXopR(elemArgs: Parser[Seq[TTerm]], op: Parser[String], funName: String, atLeastOne: Boolean)(seed: Parser[TTerm]): Parser[TTerm] =
    def buildFunTerm(argSeqs: Seq[Seq[TTerm]], seed: TTerm): TTerm =
      argSeqs.foldRight(seed) { (leftArgs, right) => applyFunction(funName, (leftArgs :+ right)*) }

    def repX[T](parser: Parser[T]): Parser[Option[T]] =
      if atLeastOne then
        parser.map(Option(_))
      else
        parser.?

    repX(Parsers.rep1sep(elemArgs, op) ~ (op ~> "..." ~>! op ~>! Parsers.rep1sep(elemArgs, op)).?) ~ seed flatMap {
      case None ~ s =>
        success(s)
      case Some((es @ List(_, _*)) ~ None) ~ s =>
        success(buildFunTerm(es, s))
      case Some((ls @ List(_, _*)) ~ Some(rs @ List(_, _*))) ~ s =>
        val startArgs = ls.last
        val endArgs = rs.head
        val seed = Some(buildFunTerm(rs.tail, s))

        extractTermRange(funName, holeArgIdx = startArgs.size, startArgs, endArgs, seed, minOccurs = 1, applyRange)
          .map(r => buildFunTerm(ls.init, r))
          .fold(msgs => err(msgs.head), success)
      case Some(Nil ~ _) ~ _ => err("impossible")
      case Some(_ ~ Some(_)) ~ _ => err("impossible")
    }  

trait TermRangeParsers extends RangeParsers[Term, Any]:

  val termBuilder = TermBuilder

trait TypeRangeParsers extends RangeParsers[Type, String]:

  val termBuilder = TypeBuilder