package tyes.cli

import tyes.model.*
import example.*

object LExpressionExtensions:
  given Conversion[LExpression, Term] with
    def apply(exp: LExpression): Term = exp match {
        case LNumber(num) => Term.Function("LNumber", Term.Constant(num))
        case LVariable(name) => Term.Function("LVariable", Term.Constant(name))
        case LPlus(left, right) => Term.Function("LPlus", left.convert, right.convert)
        case LLet(varName, varExp, inExp) => Term.Function("LLet", Term.Constant(varName), varExp.convert, inExp.convert)
    }
