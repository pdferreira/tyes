package tyes.model

enum Type:
  case Named(name: String)
  case Variable(name: String)

sealed abstract class Assertion
case class HasType(term: Term, typ: Type) extends Assertion

case class Judgement(env: Environment, assertion: Assertion)

case class RuleDecl(name: Option[String], premises: Seq[Judgement], conclusion: Judgement)
case class TypeSystemDecl(name: Option[String], rules: Seq[RuleDecl])