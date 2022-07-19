package tyes.cli

import tyes.model.*
import example.*

object LExpressionExtensions:
  given Conversion[LExpression, Term] with
    def apply(exp: LExpression): Term = exp match {
        case LNumber(num) => Term.Function("LNumber", Term.Constant(num))
        case LVariable(name) => Term.Function("LVariable", Term.Constant(name))
        case LPlus(left, right) => Term.Function("LPlus", left.convert, right.convert)
        case LLet(varName, varTypeOpt, varExp, inExp) =>
          // TODO: refactor this, duplicated with LExpressionContextParser
          val typeTerm = varTypeOpt match {
            case None => Term.Variable("$any")
            case Some(tn @ Type.Named(_)) => Term.Constant(tn)
            case Some(Type.Variable(name)) => Term.Variable(name)
          }
          Term.Function(
            "LLet",
            Term.Constant(varName),
            typeTerm,
            varExp.convert,
            inExp.convert
          )
    }
