package tyes.runtime

trait TypeSystem[E[_]]:
  type T <: Type
  def typecheck(exp: E[T], env: Environment[T]): Result[T]

  protected type Result[A <: T] = Either[String, A]

  protected def checkIf(cond: => Boolean, error: => Either[String, Unit]): Either[String, Unit] =
    if cond then
      Right(())
    else
      error

  protected def checkEnvSize(env: Environment[T], size: Int): Either[String, Unit] =
    if env.size == size then
      Right(())
    else
      TypeError.unexpectedEnvSize(env, size) 

  extension [TargetT <: T](resT: Result[TargetT])

    protected def expecting(expected: TargetT): Result[TargetT] = resT.flatMap(t =>
      if t == expected then
        Right(expected)
      else
        TypeError.unexpectedType(t, expected)
    )