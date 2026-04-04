package tyes.model.terms

enum Index:
  case Variable(name: String, min: Int = 0)
  case Number(value: Int)

  def asVariable: Option[Variable] = PartialFunction.condOpt(this) { case v: Variable => v }

  def asNumber: Option[Number] = PartialFunction.condOpt(this) { case n: Number => n }

  def fold[T](fVariable: Variable => T, fNumber: Number => T): T = this match {
    case n: Number => fNumber(n)
    case v: Variable => fVariable(v) 
  }

  def variables: Set[String] = this.asVariable.map(_.name).toSet

  def rename(subst: Map[String, String]): Index = this match {
    case Variable(name, min) => subst.get(name).map(Variable(_, min)).getOrElse(this)
    case _ => this 
  }

  override def toString: String = this match {
    case Variable(name, min) if min > 0 => s"$name[>= $min]"
    case Variable(name, _) => name
    case Number(value) => value.toString
  }
