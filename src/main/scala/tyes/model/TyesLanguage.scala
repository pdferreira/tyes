package tyes.model

enum Type:
  case Named(name: String)

enum Term:
  case Variable(name: String)
  case Function(name: String, args: Term*)

sealed abstract class Assertion
case class HasType(term: Term, typ: Type) extends Assertion

case class RuleDecl(name: Option[String], conclusion: Assertion)
case class TypeSystemDecl(name: Option[String], rules: Seq[RuleDecl])