package tyes.runtime

import scala.reflect.ClassTag
import scala.reflect.TypeTest

trait TypeOperations:

  type T <: Type

  private def getCompositeTypeArity(clazz: Class[?]): Int =
    clazz.getDeclaredConstructors() match {
      case Array(constr) => constr.getParameterCount()
      case _ => throw new Exception("Not a composite type")
    }

  private def getCompositeTypeName(clazz: Class[?]): String = clazz.getSimpleName()
  
  def destructure
    [TargetT <: CompositeType[T]]
    (typOpt: Option[T], errorMessage: String = "no type provided")
    (using tt: TypeTest[T, TargetT], ct: ClassTag[TargetT])
  : Seq[Either[String, T]] =
    val typArity = getCompositeTypeArity(ct.runtimeClass)
    typOpt match {
      case Some(typ) =>
        cast[TargetT](typ) match {
          case Right(rt) => rt.innerTypes.map(t => Right(t))
          case Left(err) => Seq.fill(typArity)(Left(err)) 
        }
      case None =>
        Seq.fill(typArity)(Left(s"TypeError: $errorMessage"))
    }

  def cast
    [TargetT]
    (t: T)
    (using tt: TypeTest[T, TargetT], ct: ClassTag[TargetT])
  : Either[String, TargetT] =
    t match {
      case t: TargetT => Right(t)
      case _ => Left(s"TypeError: not a ${getCompositeTypeName(ct.runtimeClass)}")
    }
