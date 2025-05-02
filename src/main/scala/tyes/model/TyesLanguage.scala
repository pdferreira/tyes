package tyes.model

sealed abstract class Assertion
case class HasType(term: Term, typ: Type) extends Assertion

sealed abstract class Premise
case class Judgement(env: Environment, assertion: Assertion) extends Premise
case class JudgementRange(from: Judgement, to: Judgement) extends Premise

case class RuleDecl(name: Option[String], premises: Seq[Premise], conclusion: Judgement)
case class TypeSystemDecl(name: Option[String], rules: Seq[RuleDecl])