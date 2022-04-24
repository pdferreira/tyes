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
