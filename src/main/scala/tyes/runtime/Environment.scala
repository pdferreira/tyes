package tyes.runtime

case class Environment[T <: Type](
  initialEntries: (String, T)*
):

  private val entries = Map.from(initialEntries) 

  def size: Int = entries.size

  def get(id: String): Either[String, T] = 
    entries
      .get(id)
      .map(Right.apply)
      .getOrElse(TypeError.noTypeForIdentifier(id))

  override def toString(): String =
    if entries.isEmpty
    then "no declarations"
    else entries
      .map((k, v) => s"$k: $v")
      .mkString(", ")