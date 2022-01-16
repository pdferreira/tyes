package tyes.model

trait TyesParser[E, P <: ExpressionParser[E]](expParser: P):
  import Parsers.*

  def ident = raw"[a-zA-Z][a-zA-Z\d_']*".r
  def typesystem = "typesystem" ~> ident.? ~ rule.+ ^^ { case name ~ rules => TypeSystemDecl[E](name, rules) }
  def rule = ("rule" ~> ident.?) ~ ("infers" ~> assertion) ^^ { case name ~ concl  => RuleDecl[E](name, concl) }
  def assertion: Parser[Assertion[E]] = expression ~ (":" ~> tpe) ^^ { case exp ~ tpe => HasType[E](exp, tpe) }
  def expression: Parser[E] = expParser.expression
  def tpe = ident ^^ { case name => Type.Named(name) }
  def parse(input: String) = Parsers.parse(phrase(typesystem), input)
