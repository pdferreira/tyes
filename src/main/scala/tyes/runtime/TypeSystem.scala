package tyes.runtime

import scala.reflect.ClassTag
import scala.compiletime.constValue
import scala.compiletime.ops.int.-

trait TypeSystem[E[_]]:
  type T <: Type
  def typecheck(exp: E[T], env: Environment[T]): Result[T]

  protected type Result[A <: T] = Either[String, A]

  protected type ExtractRangeRes[TTuple <: NonEmptyTuple, HoleIdx <: Int] = Option[
    Tuple.Concat[
      Tuple.Map[Tuple.Take[TTuple, HoleIdx], Seq],
      Tuple.Elem[TTuple, HoleIdx] *: Tuple.Map[Tuple.Tail[Tuple.Drop[TTuple, HoleIdx]], Seq],
    ]
  ]

  private def extractRangeFromProduct[
      TTuple <: NonEmptyTuple,
    ](
      exp: Any,
      holeIdx: Int,
      extractArgs: PartialFunction[Any, TTuple],
    ): ExtractRangeRes[TTuple, holeIdx.type] =
      def xpend: ((Any, Any)) => Seq[Any] =
        if holeIdx == 0 then { case (a, as) => as.asInstanceOf[Seq[Any]] :+ a }
        else { case (a, as) => a +: as.asInstanceOf[Seq[Any]] }

      def accumulate(tupleOfElems: Tuple, tupleOfSeqs: Tuple) = Tuple.fromArray(
        tupleOfElems.productIterator
          .zip(tupleOfSeqs.productIterator)
          .map(xpend)
          .toArray
      )

      exp match {
        case extractArgs(args) =>
          extractRangeFromProduct(args(holeIdx), holeIdx, extractArgs) match {
            case Some(tuple) =>
              Some(
                accumulate(args.take(holeIdx), tuple.take(holeIdx))
                  .asInstanceOf[Tuple.Map[Tuple.Take[TTuple, holeIdx.type], Seq]]
                ++ tuple(holeIdx).asInstanceOf[Tuple.Elem[TTuple, holeIdx.type]]
                *: accumulate(args.drop(holeIdx).tail, tuple.drop(holeIdx).tail)
                  .asInstanceOf[Tuple.Map[Tuple.Tail[Tuple.Drop[TTuple, holeIdx.type]], Seq]]
              )
            case None => 
              Some(
                args.take(holeIdx).map([A] => a => Seq(a))
                ++ args(holeIdx)
                *: args.drop(holeIdx).tail.map([A] => a => Seq(a))
              )
          }
        case _ => None
      }

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

    protected inline def extractRangeLNoSeed[P <: Product & E[T]](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: (?, ?)
      },
      ev1: ClassTag[P]
    ) =
      extractRangeFromProduct[m.MirroredElemTypes](
        exp,
        0,
        { case p: P => Tuple.fromProductTyped(p) }
      ).map {
        case seed *: rs *: EmptyTuple => (seed +: rs.asInstanceOf[Seq[?]]).asInstanceOf[Seq[Tuple.Union[m.MirroredElemTypes]]]
        case _ => ???
      }

    protected inline def extractRangeRNoSeed[P <: Product & E[T]](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: (?, ?)
      },
      ev1: ClassTag[P]
    ) =
      extractRangeFromProduct[m.MirroredElemTypes](
        exp,
        1,
        { case p: P => Tuple.fromProductTyped(p) }
      ).map {
        case ls *: seed *: EmptyTuple => (ls.asInstanceOf[Seq[?]] :+ seed).asInstanceOf[Seq[Tuple.Union[m.MirroredElemTypes]]]
        case _ => ???
      }

    protected inline def extractRangeL[P <: Product & E[T]](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: NonEmptyTuple
      },
      ev1: ClassTag[P]
    ) =
      extractRangeFromProduct[m.MirroredElemTypes](
        exp,
        0,
        { case p: P => Tuple.fromProductTyped(p) }
      )

    protected inline def extractRangeR[P <: Product & E[T]](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: NonEmptyTuple
      },
      ev1: ClassTag[P]
    ) =
      val holeIdx = constValue[Tuple.Size[m.MirroredElemTypes] - 1]
      extractRangeFromProduct[m.MirroredElemTypes](
        exp,
        holeIdx,
        { case p: P => Tuple.fromProductTyped(p) }
      )

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
