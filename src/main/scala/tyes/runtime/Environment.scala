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

  def +(newEntry: (String, T)): Environment[T] =
    this + Environment(newEntry)

  def +(otherEnv: Environment[T]): Environment[T] =
    val allEntries = entries.toSeq ++ otherEnv.entries.toSeq
    Environment(allEntries*)

  override def toString(): String =
    if entries.isEmpty
    then "no declarations"
    else entries
      .map((k, v) => s"$k: $v")
      .mkString(", ")