package tyes.cli

import scala.util.parsing.combinator.Parsers
import utils.parsers.ParserExtensions

trait OptionsParser[T]:
    
  protected object ArgParsers extends Parsers, ParserExtensions:
    override type Elem = String
  
    def anyNonFlag = any.withValidation(!_.startsWith("-"), "flag in unexpected position")
    
  protected def commandName: String

  protected def optionsSyntaxDocs: String
  
  protected def options: ArgParsers.Parser[T]

  def parse(args: Seq[String]): Either[String, T] =
    val nonEmptyArgs = args.filter(_.nonEmpty)
    if nonEmptyArgs.isEmpty then
      return Left(s"Syntax: ${commandName} ${optionsSyntaxDocs}")

    ArgParsers.parse(ArgParsers.phrase(options), nonEmptyArgs)
      .withReadableError(prefix = commandName + " ")
