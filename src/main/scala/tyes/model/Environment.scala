package tyes.model

enum Binding:
  case BindName(name: String, typ: Type)
  case BindVariable(name: String, typ: Type)

  override def toString(): String = this match {
    case BindName(name, typ) => s"C:$name -> $typ"
    case BindVariable(name, typ) => s"V:$name -> $typ"
  }

enum EnvironmentPart:
  case Bindings(bindings: Seq[Binding])
  case Variable(name: String)

  override def toString(): String = this match {
    case Bindings(bs) =>
      if bs.isEmpty then
        "<empty>"
      else
        bs.map(_.toString()).mkString(", ")
    case Variable(envVarName) =>
      envVarName
  }

case class Environment(parts: Seq[EnvironmentPart]):

  override def toString(): String =
    parts.map(_.toString()).mkString("Environment(", ", ", ")")
