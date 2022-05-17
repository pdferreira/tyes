package tyes.runtime

trait TypeSystem[E]:
  type T
  def typecheck(exp: E, env: Map[String, T]): Either[String, T]