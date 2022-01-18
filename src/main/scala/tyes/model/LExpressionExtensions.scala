package tyes.model

object LExpressionExtensions:
    given Conversion[LExpression, Term] with
        def apply(exp: LExpression): Term = exp match {
            case LNumber(num) => Term.Function("LNumber", Term.Constant(num))
        }
