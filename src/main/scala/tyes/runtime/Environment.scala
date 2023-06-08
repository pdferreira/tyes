package tyes.runtime

final case class Environment[T <: Type](
  private val entries: Map[String, T] = Map.empty[String, T]
) {

  def get(id: String): Either[String, T] = 
    entries
      .get(id)
      .map(Right.apply)
      .getOrElse(TypeError.noTypeForIdentifier(id))

}
