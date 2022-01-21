package tyes.runtime

trait TypeSystem[E]:
  type T
  def typecheck(exp: E): Either[String, T]