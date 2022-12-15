package tyes

import scala.util.parsing.combinator.RegexParsers
import utils.parsers.ParserExtensions

package object model:
  object Parsers extends RegexParsers with ParserExtensions
