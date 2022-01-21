package tyes.model

import Parsers.*

trait TermContextParserBuilder:
  def apply(metaVariableParser: Parser[Term]): Parser[Term]
