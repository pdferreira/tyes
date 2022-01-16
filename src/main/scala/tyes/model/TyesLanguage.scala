package tyes.model

enum Type:
  case Named(name: String)

sealed abstract class Assertion[E]
case class HasType[E](exp: E, typ: Type) extends Assertion[E]

case class RuleDecl[E](name: Option[String], conclusion: Assertion[E])
case class TypeSystemDecl[E](name: Option[String], rules: Seq[RuleDecl[E]])
