package tyes.runtime

object TypeError:

  private type TypeError = Either[String, Nothing]

  def apply(message: String) = generic(message)

  def generic(message: String): TypeError = Left(s"TypeError: $message")

  def noTypeFor[E](exp: E) = generic(s"no type for `$exp`")

  def noTypeForIdentifier(id: String) = generic(s"no type for identifier `$id`")

  def noTypeDeclared[E](exp: E) = generic(s"no type declared in `$exp`")

  def unexpectedType[T <: Type](obtained: T, expected: T) = 
    generic(s"expected type $obtained to be $expected instead")

  def unexpectedType[T <: Type](obtained: T, expectedClass: Class[?]) =
    val expectedType = expectedClass.getSimpleName
    generic(s"expected type $obtained to be a $expectedType instead")

  def unexpectedTerm[E](obtained: E, expected: E) =
    generic(s"expected term $obtained to be $expected instead")

  def unexpectedEnvSize[T <: Type](env: Environment[T], expectedSize: Int) =
    generic(s"expected environment with $expectedSize declarations, but found: $env")

  def allOf(errors: TypeError*): TypeError =
    Left(errors
      .flatMap(_.left.toOption)
      .mkString("All of:\n\t", "\n\t", ""))

  def oneOf(errors: TypeError*): TypeError =
    Left(errors
      .flatMap(_.left.toOption)
      .mkString("One of:\n\t", "\n\t", ""))
