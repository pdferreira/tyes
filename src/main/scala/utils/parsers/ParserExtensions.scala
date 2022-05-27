package utils.parsers

import scala.util.parsing.combinator.Parsers

trait ParserExtensions extends Parsers:
  
  def any = Parser[Elem] { in => 
    if in.atEnd
    then Failure("end of input", in)
    else Success(in.first, in.rest)
  }

  def parse[T](parser: Parser[T], elems: Seq[Elem]): ParseResult[T] =
    parser(new SeqReader(elems))

  extension [T](parser: Parser[T])
    def withValidation(cond: T => Boolean, errorMessage: String = "unexpected input state"): Parser[T] =
      Parser(in =>
        parser(in) match {
          case s: Success[T] => 
            if cond(s.result) 
            then s 
            else Error(errorMessage, in)
          case other => other 
        }
      )

    def withValidation(cond: => Boolean, errorMessage: String): Parser[T] =
      withValidation(_ => cond, errorMessage)

object ParserExtensions:

  extension [T](result: Parsers#ParseResult[T])
    
    def toOption(onNoSuccess: Parsers#NoSuccess => Unit): Option[T] = result match {
      case s: Parsers#Success[T] => 
        Some(s.result)
      case ns: Parsers#NoSuccess => 
        onNoSuccess(ns)
        None
    }

    def withReadableError: Either[String, T] = withReadableError()

    def withReadableError(prefix: String = ""): Either[String, T] = result match {
      case ns: Parsers#NoSuccess =>
        val lineIndent = " ".repeat(prefix.length)
        val errorLines = ns.next.pos.longString.linesIterator.toList
        val errorLocation = prefix + errorLines.mkString("\r\n" + lineIndent)
        val arrowPos = prefix.length + errorLines(errorLines.length - 1).length - 1
        val msgIndent = " ".repeat(Math.max(0, arrowPos - ns.msg.length / 2))
        Left(s"${errorLocation}\r\n${msgIndent}${ns.msg}")
      case s: Parsers#Success[T] => 
        Right(s.result)
    }