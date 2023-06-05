package tyes.runtime

trait TypeSystem[E[_]]:
  type T <: Type
  def typecheck(exp: E[T], env: Map[String, T]): Result[T]

  protected type Result[A <: T] = Either[String, A]

  protected def checkIf(cond: => Boolean, error: => Either[String, Unit]): Either[String, Unit] =
    if cond then
      Right(())
    else
      error

  extension [TargetT <: T](resT: Result[TargetT])

    protected def expecting(expected: TargetT): Result[TargetT] = resT.flatMap(t =>
      if t == expected then
        Right(expected)
      else
        TypeError.unexpectedType(t, expected)
    )