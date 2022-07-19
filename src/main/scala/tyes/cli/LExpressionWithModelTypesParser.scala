package tyes.cli

import example.*
import tyes.model.*
import utils.StringExtensions.*

class LExpressionWithModelTypesParser(types: Set[Type.Named]) extends LExpressionParser[Type.Named]:

  override def tpe: Parser[Type.Named] =
    types.foldLeft[Parser[Type.Named]](failure("Unrecognized type")) { (prevParser, typ) =>
      prevParser | (literal(typ.name) ~> success(typ))
    }
