package tyes.runtime

trait TypeSystem[E[_]]:
  type T
  def typecheck(exp: E[T], env: Map[String, T]): Either[String, T]