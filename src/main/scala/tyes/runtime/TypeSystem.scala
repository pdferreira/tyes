package tyes.runtime

import scala.reflect.ClassTag

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

  protected def checkTypeDeclared(typOpt: Option[T], parentExp: E[T]): Result[T] =
    typOpt
      .map(Right.apply)
      .getOrElse(TypeError.noTypeDeclared(parentExp))

  extension (exp: E[T])

    protected def expecting[TargetT <: E[T]](using ct: ClassTag[TargetT]): Either[String, TargetT] =
      ct.unapply(exp)
        .map(Right.apply)
        .getOrElse(TypeError.noTypeFor(exp))

    protected def extractRange(extractArgs: PartialFunction[E[T], (E[T], E[T])]): Option[Seq[E[T]]] = exp match {
      case extractArgs(ls, r) =>
        ls.extractRange(extractArgs) match {
          case Some(es) => Some(es :+ r)
          case None => Some(Seq(ls, r))
        }
      case _ => None
    }

  extension (resT: Result[T])

    protected def expecting[TargetT <: T](expected: TargetT): Result[TargetT] = resT.flatMap(t =>
      if t == expected then
        Right(expected)
      else
        TypeError.unexpectedType(t, expected)
    )

    protected def expecting[TargetT <: T](using ct: ClassTag[TargetT]) = resT.flatMap(t =>
      ct.unapply(t)
        .map(Right.apply)
        .getOrElse(TypeError.unexpectedType(t, ct.runtimeClass))
    )

  extension [A](seq: Seq[A])

    protected def foldRange[B](init: Seq[B])(f: A => Either[String, B]): Either[String, Seq[B]] =
      seq.foldLeft(Right(init).withLeft[String]) { (accR, curr) => for
        acc <- accR
        res <- f(curr)
      yield
        acc :+ res
      }

