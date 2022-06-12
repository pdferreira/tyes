package tyes.model

enum Type:
  case Named(name: String)
  case Variable(name: String)

sealed abstract class Assertion
case class HasType(term: Term, typ: Type) extends Assertion

enum Binding:
  case BindName(name: String, typ: Type)
  case BindVariable(name: String, typ: Type)

enum EnvironmentPart:
  case Bindings(bindings: Seq[Binding])
  case Variable(name: String)

case class Environment(parts: Seq[EnvironmentPart])

case class Judgement(env: Environment, assertion: Assertion)

case class RuleDecl(name: Option[String], premises: Seq[Judgement], conclusion: Judgement)
case class TypeSystemDecl(name: Option[String], rules: Seq[RuleDecl])