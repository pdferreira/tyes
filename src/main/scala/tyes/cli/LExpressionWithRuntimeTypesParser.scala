package tyes.cli

import example.*
import utils.StringExtensions.*

class LExpressionWithRuntimeTypesParser[T <: tyes.runtime.Type](types: Set[T]) extends LExpressionParser[T]:

  override def tpe: Parser[T] =
    types.foldLeft[Parser[T]](failure("Unrecognized type")) { (prevParser, typ) =>
      prevParser | (literal(typ.toString.decapitalize) ~> success(typ))
    }
