package tyes.cli

import tyes.model.*
import example.*

object LExpressionExtensions:

  given Conversion[LExpression[Type], Term] with
  
    def apply(exp: LExpression[Type]): Term = exp match {
        case LNumber(num) => Term.Function("LNumber", Term.Constant(num))
        case LVariable(name) => Term.Function("LVariable", Term.Constant(name))
        case LPlus(left, right) => Term.Function("LPlus", left.convert, right.convert)
        case LLet(varName, varTypeOpt, varExp, inExp) =>
          Term.Function(
            "LLet",
            Term.Constant(varName),
            Term.Type(varTypeOpt.getOrElse(Constants.Types.any)),
            varExp.convert,
            inExp.convert
          )
    }
