package utils.collections

import scala.reflect.ClassTag

extension [A](it: Iterable[A])

  def foldLeft1(op: (A, A) => A): A = it.tail.foldLeft(it.head)(op)

  def foldRight1(op: (A, A) => A): A = it.dropRight(1).foldRight(it.last)(op)

  def mapWithContext[B, C](ctx: C)(f: (C, A) => (C, B)): Seq[B] = it match {
    case Nil => Nil
    case head :: next => 
      val (newCtx, b) = f(ctx, head)
      b +: next.mapWithContext(newCtx)(f)
  }

  def mkStringOrEmpty(start: String, sep: String, end: String): String =
    if it.isEmpty
    then ""
    else it.mkString(start, sep, end)

  def nonEmptyOption: Option[it.type] =
    if it.isEmpty
    then None
    else Some(it)

  def ofType[T: ClassTag]: Iterable[T] =
    it.collect({ case v: T => v })
