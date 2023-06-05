package tyes.runtime

object TypeError:

  private type TypeError = Either[String, Nothing]

  def apply(message: String) = generic(message)

  def generic(message: String): TypeError = Left(s"TypeError: $message")

  def noTypeFor[E](exp: E) = generic(s"no type for `$exp`")

  def unexpectedType[T <: Type](obtained: T, expected: T) = 
    generic(s"expected type $obtained to be $expected instead")
