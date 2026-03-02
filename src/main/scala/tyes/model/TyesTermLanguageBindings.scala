package tyes.model

import Parsers.*
import tyes.model.indexes.*
import tyes.model.ranges.*
import scala.annotation.targetName

trait TyesTermLanguageBindings:

  def metaTermVariableParser: Parser[Term]
  
  def identTermParser(ident: String): Parser[Term]

  def typeParser: Parser[Type]

  def rep1opL(elem: Parser[Term], op: Parser[String], funName: String): Parser[Term] =
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
            Some(ls.tail.init.foldLeft(ls.head) { (left, right) => Term.Function(funName, left, right) })

        extractTermRange(funName, holeArgIdx = 0, Seq(start), Seq(end), seed, minOccurs = 2, Term.Range.apply)
          .map(r => rs.foldLeft(r: Term) { (left, right) => Term.Function(funName, left, right) })
          .fold(msgs => err(msgs.head), success)
      case Nil ~ _ => err("impossible")
      case _ ~ Some(_) => err("impossible")
    }

  def rep0opR(elem: Parser[Term], op: Parser[String], funName: String)(seed: Parser[Term]): Parser[Term] =
    repXopR(elem.map(Seq(_)), op, funName, atLeastOne = false)(seed)

  def repXopR(elemArgs: Parser[Seq[Term]], op: Parser[String], funName: String, atLeastOne: Boolean)(seed: Parser[Term]): Parser[Term] =
    def buildFunTerm(argSeqs: Seq[Seq[Term]], seed: Term): Term =
      argSeqs.foldRight(seed) { (leftArgs, right) => Term.Function(funName, (leftArgs :+ right)*) }

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

        extractTermRange(funName, holeArgIdx = startArgs.size, startArgs, endArgs, seed, minOccurs = 1, Term.Range.apply)
          .map(r => buildFunTerm(ls.init, r))
          .fold(msgs => err(msgs.head), success)
      case Some(Nil ~ _) ~ _ => err("impossible")
      case Some(_ ~ Some(_)) ~ _ => err("impossible")
    }
