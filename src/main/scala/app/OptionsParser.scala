package app

import scala.util.parsing.combinator.Parsers
import utils.parsers.ParserExtensions

trait OptionsParser[T]:
    
  protected object ArgParsers extends Parsers, ParserExtensions:
    override type Elem = String
  
    def anyNonFlag = any.withValidation(!_.startsWith("-"), "flag in unexpected position")
    
  protected def commandName: String

  protected def optionsSyntaxDocs: String
  
  protected def options: ArgParsers.Parser[T]

  def parse(args: Seq[String]): Option[T] = 
    if args.isEmpty then
      Console.err.println(s"Syntax: ${commandName} ${optionsSyntaxDocs}")
      return None

    (ArgParsers.parse(ArgParsers.phrase(options), args): @unchecked) match {
      case ArgParsers.NoSuccess(msg, next) => 
        val cmdPrefix = commandName + " "
        val lineIndent = " ".repeat(cmdPrefix.length)
        val errorLocation = cmdPrefix + next.pos.longString.linesIterator.mkString("\r\n" + lineIndent)
        val middlePos = next.pos.column + cmdPrefix.length
        val msgIndent = " ".repeat(Math.max(0, middlePos - msg.length / 2))
        Console.err.println(s"${errorLocation}\r\n${msgIndent}${msg}")
        None
      case ArgParsers.Success(res, _) => Some(res)
    }
