package tyes.runtime

import scala.reflect.ClassTag
import scala.compiletime.constValue
import scala.compiletime.ops.int.-
import utils.collections.*

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
      matchesSeed: Tuple.Elem[TTuple, holeIdx.type] => Boolean = (_: Any) => true
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
          extractRangeFromProduct(args(holeIdx), holeIdx, extractArgs, matchesSeed) match {
            case Some(tuple) =>
              Some(
                accumulate(args.take(holeIdx), tuple.take(holeIdx))
                  .asInstanceOf[Tuple.Map[Tuple.Take[TTuple, holeIdx.type], Seq]]
                ++ tuple(holeIdx).asInstanceOf[Tuple.Elem[TTuple, holeIdx.type]]
                *: accumulate(args.drop(holeIdx).tail, tuple.drop(holeIdx).tail)
                  .asInstanceOf[Tuple.Map[Tuple.Tail[Tuple.Drop[TTuple, holeIdx.type]], Seq]]
              )
            case _ if matchesSeed(args(holeIdx)) => 
              Some(
                args.take(holeIdx).map([A] => a => Seq(a))
                ++ args(holeIdx)
                *: args.drop(holeIdx).tail.map([A] => a => Seq(a))
              )
            case _ =>
              None
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

  extension [X](term: X)

    protected inline def extractRangeLNoSeed[P <: Product & X](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: (?, ?)
      },
      ev1: ClassTag[P]
    ) =
      extractRangeFromProduct[m.MirroredElemTypes](
        term,
        0,
        { case p: P => Tuple.fromProductTyped(p) }
      ).map {
        case seed *: rs *: EmptyTuple => (seed +: rs.asInstanceOf[Seq[?]]).asInstanceOf[Seq[Tuple.Union[m.MirroredElemTypes]]]
        case _ => ???
      }

    protected inline def extractRangeRNoSeed[P <: Product & X](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: (?, ?)
      }
    ) =
      extractRangeFromProduct[m.MirroredElemTypes](
        term,
        1,
        { case p: P => Tuple.fromProductTyped(p) }
      ).map {
        case ls *: seed *: EmptyTuple => (ls.asInstanceOf[Seq[?]] :+ seed).asInstanceOf[Seq[Tuple.Union[m.MirroredElemTypes]]]
        case _ => ???
      }

    protected inline def extractRangeRNoSeedCustom[P <: Product](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: (?, ?)
      },
      ev0: Tuple.Elem[m.MirroredElemTypes, 0] =:= Tuple.Union[m.MirroredElemTypes],
      ev1: Tuple.Elem[m.MirroredElemTypes, 1] =:= Tuple.Union[m.MirroredElemTypes]
    )[P2 <: Product](
      matchTemplate: PartialFunction[Tuple.Union[m.MirroredElemTypes], P2]
    )(using
      m2: deriving.Mirror.ProductOf[P2] {
        type MirroredElemTypes <: NonEmptyTuple
      }
    ) =
      extractRangeFromProduct[m.MirroredElemTypes](
        term,
        1,
        Function.unlift({
          case p: P =>
            val tuple = Tuple.fromProductTyped(p)
            // validate non-hole arguments match the expected template (in this case just #0)
            if matchTemplate.isDefinedAt(tuple(0)) then
              Some(tuple)
            else
              None
          case _ => None
        }),
        // validate hole argument matches the expected template
        matchTemplate.isDefinedAt,
      ).map {
        case ls *: seed *: EmptyTuple => (ls.asInstanceOf[Seq[?]] :+ seed)
          .asInstanceOf[Seq[Tuple.Union[m.MirroredElemTypes]]]
          .map(matchTemplate)
          .map(Tuple.fromProductTyped)
          .unzipN
        case _ => ???
      }

    protected inline def extractRangeL[P <: Product & X](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: NonEmptyTuple
      },
      ev1: ClassTag[P]
    ) =
      extractRangeFromProduct[m.MirroredElemTypes](
        term,
        0,
        { case p: P => Tuple.fromProductTyped(p) }
      )

    protected inline def extractRangeR[P <: Product & X](using
      m: deriving.Mirror.ProductOf[P] {
        type MirroredElemTypes <: NonEmptyTuple
      },
      ev1: ClassTag[P]
    ) =
      val holeIdx = constValue[Tuple.Size[m.MirroredElemTypes] - 1]
      extractRangeFromProduct[m.MirroredElemTypes](
        term,
        holeIdx,
        { case p: P => Tuple.fromProductTyped(p) }
      )

  extension (t: T)

    protected def expecting[TargetT <: T](expected: TargetT): Result[TargetT] =
      if t == expected then
        Right(expected)
      else
        TypeError.unexpectedType(t, expected)

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
