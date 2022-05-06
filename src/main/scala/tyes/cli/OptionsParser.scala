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
    if args.isEmpty then
      return Left(s"Syntax: ${commandName} ${optionsSyntaxDocs}")

    (ArgParsers.parse(ArgParsers.phrase(options), args): @unchecked) match {
      case ArgParsers.NoSuccess(msg, next) => 
        val cmdPrefix = commandName + " "
        val lineIndent = " ".repeat(cmdPrefix.length)
        val errorLines = next.pos.longString.linesIterator.toList
        val errorLocation = cmdPrefix + errorLines.mkString("\r\n" + lineIndent)
        val arrowPos = cmdPrefix.length + errorLines(errorLines.length - 1).length - 1
        val msgIndent = " ".repeat(Math.max(0, arrowPos - msg.length / 2))
        return Left(s"${errorLocation}\r\n${msgIndent}${msg}")
      case ArgParsers.Success(res, _) => 
        return Right(res)
    }
