package utils.collections

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
