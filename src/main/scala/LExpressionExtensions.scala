import tyes.model.*
import example.*

object LExpressionExtensions:
  given Conversion[LExpression, Term] with
    def apply(exp: LExpression): Term = exp match {
        case LNumber(num) => Term.Function("LNumber", Term.Constant(num))
        case LPlus(left, right) => Term.Function("LPlus", left.convert, right.convert)
    }
