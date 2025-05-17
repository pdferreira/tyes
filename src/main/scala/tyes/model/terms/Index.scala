package tyes.model.terms

enum Index:
  case Variable(name: String, min: Int = 0)
  case Number(value: Int)

  def asVariable: Option[Variable] = PartialFunction.condOpt(this) { case v: Variable => v }

  def asNumber: Option[Number] = PartialFunction.condOpt(this) { case n: Number => n }

  override def toString: String = this match {
    case Variable(name, min) if min > 0 => s"$name[>= $min]"
    case Variable(name, _) => name
    case Number(value) => value.toString
  }
