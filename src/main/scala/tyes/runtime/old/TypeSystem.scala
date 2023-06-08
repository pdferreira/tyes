package tyes.runtime.old

import tyes.runtime.Type

trait TypeSystem[E[_]]:
  type T <: Type
  def typecheck(exp: E[T], env: Map[String, T]): Either[String, T]
