package tyes.model.terms

import scala.reflect.ClassTag

trait SpecializedTerm[Self, TTerm <: TermOps[TTerm, TConstant], TConstant](
  val specialized: TTerm
)(using sClass: ClassTag[TTerm]):
  this: Self =>

  def copy(specialized: TTerm = specialized): Self =
    getClass
      .getMethod("apply", sClass.runtimeClass)
      .invoke(null, specialized)
      .asInstanceOf[Self]